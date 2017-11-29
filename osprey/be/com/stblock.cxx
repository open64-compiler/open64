/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * $Revision: 1.10 $
 * $Date: 05/12/05 08:59:14-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.stblock.cxx $
 *
 * Revision history:
 *  11-Nov-94 - Original Version
 *
 * Description:
 *
 * General support for ST blocks and bases.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <cmplrs/rcodes.h>
#ifndef __MINGW32__
#include <sys/resource.h>
#endif /* __MINGW32__ */
#include "defs.h"
#include "config_asm.h"
#include "erglob.h"
#include "erbe.h"
#include "config.h"
#include "tracing.h"
#include "strtab.h"
#include "stab.h"
#include "const.h"
#include "glob.h"
#include "mtypes.h"
#include "targ_const.h"
#include "targ_sim.h"
#include "ttype.h"
#include "irbdata.h"
#include "util.h"
#include "stblock.h"
#include "data_layout.h"

#define SYMTAB_level(s)	s

#define Has_No_Base_Block(st) (ST_base(st) == st)

#define ROUNDUP(val,align) 	( (-(INT64)align) & (INT64)(val+align-1) )
#define ROUNDDOWN(val,align)	( (-(INT64)align) & (INT64)(val)         )


/* ====================================================================
 *
 * Adjusted_Alignment
 *
 * Adjust the alignment during data layout for all objects by either
 *	ST_use_reg_align()  or  Aggregate_Alignment
 *
 * ====================================================================
 */
extern INT16
Adjusted_Alignment(ST *sym)
{
  if (ST_class(sym) == CLASS_BLOCK)
      return STB_align(sym);

  INT32  align;
  TY_IDX ty_idx = ST_type(sym);
  TY& ty = Ty_Table[ST_type (sym)];

  switch(ST_class(sym))
  {
  case CLASS_VAR:
    align=	TY_align(ty_idx);

    if (Is_Structure_Type(ty) && TY_is_packed(ty))
    {
      return align;
    }

    if (TY_is_user_align(ty_idx))
    {
      return align;
    }

    switch(ST_sclass(sym))
    {
    case SCLASS_AUTO:
    case SCLASS_PSTATIC:
    case SCLASS_FSTATIC:
    case SCLASS_DGLOBAL:
    case SCLASS_UGLOBAL:
    case SCLASS_COMMON:

     /*
      *  variables of this class can have at least Aggregate_Alignment
      */
      if (Is_Composite_Type(ty))
      {
	/* datapool's only have one element, so use existing alignment */
	if (ST_is_datapool(sym)) {
		break;
	}
	else {
		align = MAX(align, Aggregate_Alignment);
	}
      }
      break;

    case SCLASS_FORMAL:
     /*
      *  the offsets for these are predetermined.
      *  You cannot change it, or you will affect upformals
      */
      break;
    }
#if defined(TARG_IA32) || defined(TARG_X8664)
    // Stack-based objects should not have alignment greater than the
    // stack alignment. This is a temporary kludge for that, to keep
    // ia32 development going. The real fix may require FE changes.

    // We might need to fix Machine_Types[] defined in mtype.cxx
    if (ST_sclass(sym) == SCLASS_AUTO || ST_sclass(sym) == SCLASS_FORMAL) {
      INT16 stack_align = Stack_Alignment();
      if (align > stack_align) {
        align = stack_align;
      }
    }

    /* If -TENV:align_double is ON, then align double and long double
       stack variables on a double word boundary.
    */
    if( ST_sclass(sym) == SCLASS_AUTO &&
	Align_Double ){
      if( ( TY_mtype(ty) == MTYPE_FQ || TY_mtype(ty) == MTYPE_F10 ||
	    TY_mtype(ty) == MTYPE_F8 ) &&
	  ( align < 8 ) ){
	Is_True( Stack_Alignment() >= 8, ("stack is not double word aligned") );
	align = 8;
      }
    }
#endif
    return align ;
 
  case CLASS_CONST:
    if (TCON_ty(STC_val(sym)) == MTYPE_STR && TY_kind(ty) == KIND_POINTER)
    {
     /*
      *  can be either char string or wchar string; use element alignment. 
      *  please note we cannot use ST_use_reg_align() on a const
      */
      align =	TY_align(TY_pointed(ty));
    }
    else
    {
      align =	TY_align(ty_idx);
    }

    if (TY_kind(ty) != KIND_SCALAR) {
	/*
	 * allow users to specify const alignment of 
	 * composite and string types.
	 * (could also use Is_Composite_Type or is-string-type).
	 */
	align = MAX(align, Aggregate_Alignment);
    }

   /*
    *  let string fall into their natural alignment class
    *	 ex	size (1 -> 1), (2 ->2), (3,4 -> 4) , etc
    */
#ifdef KEY
    /* Under -LANG:global_asm, the ASMs may have .section attributes,
     * so the compiler cannot be sure about offset/alignment of
     * compiler allocated objects, so use minimum alignment (bug 14506) */
    if ( /* Optimize_Space==FALSE */ LANG_Enable_Global_Asm == FALSE ||
         FILE_INFO_has_global_asm(File_info))
#else
    if ( /* Optimize_Space==FALSE */ TRUE )
#endif
    {
      INT64 size = ST_size(sym);

      if (size)
      { 
	if(size < MTYPE_align_best(Spill_Int_Mtype))
	{
	  INT32 pow2= nearest_power_of_two(size);

	  align = MAX(align, pow2);
	}
	else
	{
	  align = MAX(align, MTYPE_align_best(Spill_Int_Mtype));
	}
      }
    }
    return align;
  }

  return TY_align(ty_idx);
}




/* ====================================================================
 *
 * ST_alignment
 *
 * Returns alignment for the object. 
 * The object can have an improved alignment then the TY_align() if 
 * already allocated.
 *
 * ====================================================================
 */
INT32 ST_alignment(ST *sym)
{
  INT32 align=	Adjusted_Alignment(sym);

  if (ST_pu_defined(sym))
  {
    if (Is_Allocated(sym))
    {
      ST	*base;
      INT64	ofst;
      INT32	basealign;

      Base_Symbol_And_Offset(sym, &base, &ofst);

      Is_True((sym != base), ("sym should != base"));

      basealign=	Adjusted_Alignment(base);

      Is_True((basealign>=align), ("sym has align > than base"));

#ifdef TARG_NVISA
      // When we emit ptx we ignore the allocation offset
      // and emit each variable separately, then let OCG 
      // assign addresses.  So it is not safe to change alignment
      // based on the offset and segment.
      return align;
#endif

      while (basealign > align)
      {
	if ((ofst % basealign) == 0)
	{
	  if (Get_Trace(TP_LOWER, 4))
	  {
	    DevWarn("ST_alignment: Using allocated alignment for %s (prev align %d) (new align %d)",
		    ST_name(sym), align, basealign);
	  }
	  return basealign;
	}
	basealign >>=	1;
      }
      return align;
    }
    return align;
  }
 
  return align;
}


/* ====================================================================
 *
 * New_ST_Block
 *
 * Allocate a ST entry of CLASS_BLOCK, set up appropriate flags.
 *
 * ====================================================================
 */
extern ST*
New_ST_Block (STR_IDX name, BOOL is_global, ST_SCLASS sclass, UINT16 align, INT64 size)
{
  ST *new_blk = New_ST(is_global ? GLOBAL_SYMTAB : CURRENT_SYMTAB);
  ST_Init (new_blk, name, CLASS_BLOCK, sclass, EXPORT_LOCAL, 0);
  BLK_IDX blk_idx;
  (void) New_BLK(blk_idx);
  Set_ST_blk(new_blk, blk_idx);
  Set_STB_align(new_blk, align);
  Set_STB_size(new_blk, size);
  return new_blk;
}

extern ST*
Copy_ST_Block (ST *orig_blk)
{
  ST *new_blk = New_ST_Block (ST_name_idx(orig_blk), 
	ST_level(orig_blk) == GLOBAL_SYMTAB, 
	ST_sclass(orig_blk), STB_align(orig_blk), STB_size(orig_blk) );
  Set_STB_flags(new_blk, STB_flags(orig_blk));
  Set_STB_section_idx(new_blk, STB_section_idx(orig_blk));
  Set_STB_scninfo_idx(new_blk, STB_scninfo_idx(orig_blk));
  return new_blk;
}


/* ====================================================================
 *
 * Create_And_Set_ST_Base
 *
 * Set the ST_base for two blocks that are merged into one.  It
 * identify the container block, we call base, by checking the ST_base
 * of blk1.  If the ST_base is pointing to itself, that means the
 * container block has not be defined yet.  We create a new container
 * block for it.  The second block should not have a container block.
 * After we have identify the container block, we set the ST_base of
 * both blocks with this container block.
 *
 * ====================================================================
 */
static ST *
Create_And_Set_ST_Base(ST *blk1, ST *blk2, STACK_DIR dir)
{
  ST *base;
  ST *blk1_base = Base_Symbol(blk1);
  if (ST_class(blk1_base) != CLASS_BLOCK) {
      base = New_ST_Block (Save_Str2(ST_name(blk1_base),".BLOCK"), 
	Is_Global_Symbol(blk1_base), 
	ST_sclass(blk1_base), Adjusted_Alignment(blk1_base), 
	ST_size(blk1_base));
      if (dir == DECREMENT) Set_STB_decrement(base);
      Set_ST_base(blk1_base, base);
      Enter_ST(base);
  }
  else if (ST_class(blk1) == CLASS_BLOCK) {
    base = blk1;
  } else {
    base = blk1_base;
  }

  Set_ST_base(blk2, base);
  return base;
}



/*===================================================================
 *
 * St_Block_Union
 *
 * Force two objects to use the same memory.
 * Merge second ST entry into the first ST entry's container block,
 * if these two ST entries belong to the same storage class.  It
 * creates a new container block for the first ST if there isn't one
 * yet. After setting the base, it performs data layout by assigning
 * both offsets to 0 (modulo alignment)
 *
 * ====================================================================
 */

void St_Block_Union(ST *blk1, ST *blk2)
{
  ST *base;
  INT64 size1,size2;

  Is_True(!ST_is_initialized(blk2),
          ("St_Block_Union: union an initialized ST is not allowed"));
  Is_True(ST_sclass(blk1) == ST_sclass(blk2),
	  ("Block_Union: Different SCLASS for %s and %s",
		ST_name(blk1), ST_name(blk2) ) );

  base = Create_And_Set_ST_Base(blk1,blk2, INCREMENT);

  size1 = ST_size(base);
  size2 = ST_size(blk2);

  Set_STB_align(base, MAX(STB_align(base), Adjusted_Alignment(blk2)));
  Set_STB_size(base, ROUNDUP(MAX(size1,size2),STB_align(base)));
  Set_ST_ofst(blk2,0);

  if (Is_Global_Symbol(base)) {
	// global symbols are allocated at beginning of be,
	// and routines like Process_Bss_Data expect global allocation
	// done at end of each PU, so allocat base now.
	Allocate_Object(base);
  }

  if ( Get_Trace(TP_DATALAYOUT, 1)) {
	fprintf(TFile, "union %s with %s, base = %s\n",
		ST_name(blk1), ST_name(blk2), ST_name(base) );
  }
}


/* ====================================================================
 *
 * Offset_From_Base_Symbol
 *
 * Follow the chain of ST_base symbols and return the offset
 * from the "final" base symbol
 *
 * ====================================================================
 */
INT64
Offset_From_Base_Symbol ( ST *st )
{
  INT64 ofst;
  ST    *base;

  Base_Symbol_And_Offset ( st, &base, &ofst );
  return ofst;
}

/* ====================================================================
 *
 * Base_Symbol
 *
 * Follow the chain of ST_base symbols and return the "final" base symbol
 *
 * ====================================================================
 */

ST *
Base_Symbol (ST *st) 
{
  INT64 ofst;
  ST    *base;

  Base_Symbol_And_Offset ( st, &base, &ofst );
  return base;
}


// Create slink symbol in current PU
void
Create_Slink_Symbol (void)
{
	// only create slink if a nested function.
	if ( ! PU_is_nested_func(Get_Current_PU())) return;
  // For MP-lowering process, the PU created by compiler
  // May have created one. added by csc
  if ( Find_Slink_Symbol( CURRENT_SYMTAB ) != NULL ) return;

	ST *st = Gen_Temp_Symbol (MTYPE_To_TY(Pointer_type), "__slink_sym");
}

// return ST for __slink_sym if one exists in specified PU.

struct is_slink_sym
{
    BOOL operator () (UINT32, const ST *st) const {
	return (strncmp (ST_name (st), 
			 Temp_Symbol_Prefix "__slink_sym", 
			 sizeof(Temp_Symbol_Prefix "__slink_sym") - 1 ) == 0);
    }
};

ST *
Find_Slink_Symbol (SYMTAB_IDX stab)
{
    if (!PU_is_nested_func (Get_Scope_PU (stab)))
	return NULL;

    ST_IDX st_idx = For_all_until (St_Table, stab, is_slink_sym ());

    if (st_idx == 0)
	return NULL;
    else
	return &St_Table[st_idx];
} // Find_Slink_Symbol


/* ====================================================================
 *
 * Find_Slink_Sym_For_ST(ST *st)
 *
 * Return the slink_sym that can reference this st.
 *
 * The slink_sym will be in the symtab of the child 
 *
 * ====================================================================
 */

ST *Find_Slink_For_ST(ST *st)
{
  UINT level = ST_level(st);
  SYMTAB_IDX stab= CURRENT_SYMTAB;

  while(stab)
  {
    SYMTAB_IDX parent = SYMTAB_parent(stab);

    if (parent && (SYMTAB_level(parent) == level))
    {
      ST	*slink = Find_Slink_Symbol(stab);
      Is_True ((slink), ("no SYMTAB_slink_sym() "));
      return slink;
    }
    stab = parent;
  }

  FmtAssert(FALSE,
	   ("Find_Slink_For_ST() cannot find symtab for %s (level=%d)",
	     ST_name(st), ST_level(st)));
  /* NOTREACHED */
  return NULL;
}


/* ====================================================================
 *
 * Find_Slink_For_Scope(SYMTAB_IDX symtab, INT32 scope_id)
 *
 * Return the slink_sym at scope_id
 *
 * ====================================================================
 */

ST *Find_Slink_For_Scope(ST *function, ST *sym)
{
  SYMTAB_IDX symtab= CURRENT_SYMTAB;
  INT32 level = PU_lexical_level (function) - PU_lexical_level (sym);

  while(level-- > 0)
  {
    symtab= SYMTAB_parent(symtab);

    Is_True ((symtab), ("no symtab at correct level"));
  }

  {
    ST	*slink = Find_Slink_Symbol (symtab);
    Is_True ((slink), ("no SYMTAB_slink_sym() "));
    return slink;
  }
  /* NOTREACHED */
  return NULL;
}

BOOL ST_is_uplevelTemp(const ST *st)
{
  if (ST_class(st) != CLASS_VAR		&&
      ST_class(st) != CLASS_BLOCK)
    return FALSE;

  if (Is_Global_Symbol(st) || Is_Local_Symbol(st)) 
    return FALSE;

  if (PU_has_altentry(Get_Current_PU()) && ST_declared_static(st))
    return FALSE;

  switch(ST_sclass(st))
  {
  case SCLASS_AUTO:
  case SCLASS_FORMAL:
    return TRUE;
  }

  return FALSE;
}
#ifdef KEY // for supporting label being jumped to from a nested function
	   // function containing a label being jumped to from a nested 
	   // function needs to have save locations for its FP and SP

ST *
Create_FPSave_Symbol (void)
{
  return Gen_Temp_Symbol(MTYPE_To_TY(Pointer_type), "__fpsave_sym");
}

struct is_fpsave_sym
{
    BOOL operator () (UINT32, const ST *st) const {
	return (strncmp (ST_name (st), 
			 Temp_Symbol_Prefix "__fpsave_sym", 
			 sizeof(Temp_Symbol_Prefix "__fpsave_sym") - 1) == 0);
    }
};

ST *
Find_FPSave_Symbol (SYMTAB_IDX stab)
{
    if (!PU_has_nonlocal_goto_label(Get_Scope_PU (stab)))
	return NULL;

    ST_IDX st_idx = For_all_until (St_Table, stab, is_fpsave_sym ());

    if (st_idx == 0)
	return NULL;
    else
	return &St_Table[st_idx];
} // Find_FPSave_Symbol

ST *
Create_SPSave_Symbol (void)
{
  return Gen_Temp_Symbol(MTYPE_To_TY(Pointer_type), "__spsave_sym");
}

struct is_spsave_sym
{
    BOOL operator () (UINT32, const ST *st) const {
	return (strncmp (ST_name (st), 
			 Temp_Symbol_Prefix "__spsave_sym",
			 sizeof(Temp_Symbol_Prefix "__spsave_sym") - 1 ) == 0);
    }
};

ST *
Find_SPSave_Symbol (SYMTAB_IDX stab)
{
    if (!PU_has_nonlocal_goto_label(Get_Scope_PU (stab)))
	return NULL;

    ST_IDX st_idx = For_all_until (St_Table, stab, is_spsave_sym ());

    if (st_idx == 0)
	return NULL;
    else
	return &St_Table[st_idx];
} // Find_SPSave_Symbol
#endif


/* ====================================================================
 *
 * Base_Symbol_And_Offset_For_Addressing
 *	Input:  ST *sym			 Symbol to analyze
 *	Input:  INT64 ofst		 Offset from sym
 *      Result: ST **base_symbol	 base of st for addressing
 *      Result: INT64 *offset_from_base	 offset from base
 *
 * This routine returns the <base_symbol> and <offset_from_base> that
 * should be used for addressing. There are some cases where we cannot
 * use the ST_base (for text symbols and preemptible symbols). For those
 * cases, we simply return the <sym> and <ofst> themselves.
 *
 * ====================================================================
 */
void
Base_Symbol_And_Offset_For_Addressing (
  ST *sym,
  INT64 ofst,
  ST **base_symbol,
  INT64 *offset_from_base)
{
  /* 1. For text symbols, don't use the base_sym and base_ofst because
   *    they may not have been set yet. This happens for a forward
   *    reference to a text address.
   * 2. For preemptible symbols, we cannot use the base where the symbol
   *    is allocated since it could be preempted. 
   */
#ifdef KEY
  /* 3. For weak symbol, we cannot use the base where the symbol
        is allocated since weak symbol could be defined in another file.
	(bug#3052)
     4. For thread-local symbols, don't use the base where the symbol is
	allocated.
   */
#endif // KEY

  INT64 tofst = 0;
  ST *base = sym;

  while( (ST_base(base) != base  ) 
	 && (ST_sclass(base) != SCLASS_TEXT) 
	 && !((Gen_PIC_Shared || Gen_PIC_Call_Shared) && !ST_is_export_local(base))
#ifdef KEY
	 && !ST_is_weak_symbol(base)
	 && !ST_is_thread_local(base)
#endif // KEY
	 )
  {
      tofst += ST_ofst(base);
      base = ST_base(base); 
  }

  *base_symbol      = base;
  *offset_from_base = tofst + ofst;
}

