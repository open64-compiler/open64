/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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
 * Module: iface_scn.c
 * $Revision: 1.10 $
 * $Date: 05/12/05 08:59:08-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.iface_scn.cxx $
 *
 * Revision history:
 *  15-Jun-94 - Original version
 *
 * Description:
 *
 * Handle the .interfaces section for the emitter
 *
 * ====================================================================
 * ====================================================================
 */

#if ! defined(BUILD_OS_DARWIN)

#if defined(__GNUC__)
#include <sys/types.h>
#include <stdio.h>
#endif
#include <bstring.h>
#ifdef TARG_IA64
#include <elf_stuff.h>
#include <libelf/libelf.h>
#else
#include "elf_stuff.h"
#include "libelf/libelf.h"
#endif
#include <elfaccess.h>
#include <stdlib.h>
#include <cmplrs/elf_interfaces.h>

#define	USE_STANDARD_TYPES 1
#include "defs.h"
#include "config.h"
#include "config_debug.h"
#include "symtab.h"

#include "erglob.h"
#include "erlib.h"
#include "ercg.h"
#include "flags.h"
#include "glob.h"
#include "tracing.h"

#include "tn.h"
#include "calls.h"
#include "targ_sim.h"
#include "ttype.h"
#include "em_elf.h"
#include "cgemit.h"
#include "iface_scn.h"


/* ====================================================================
 *
 * Local data
 *
 * ====================================================================
 */

/* Most of these structures are closely related to the structures
 * described in "64-bit ELF Object File Specification".
 */

typedef struct parm_descriptor {
  mUINT8 flags;	       	        /* flags */
  mUINT8 qual_count;		/* count of qualifiers */
  mUINT8 fundamental_type;	/* the fundamental type */
  mUINT8 qualifiers[16]; 	/* type qualifiers */
  mINT32 size;			/* type size if indeterminate */
  struct parm_descriptor *next;	/* next parm descriptor in list */
} PARM_DESCRIPTOR;

/* note: used array of qualifiers because the overhead of an allocated
 * block of memory may end up being as great as the number of bytes
 * we'd ever allocate.
 */

typedef struct parm_profile {
  mINT16 var_size;		/* the size in bytes of this profile */
  mINT16 parm_count;		/* # of parm type descriptors */
  PARM_DESCRIPTOR *result;	/* optional result type descriptor */
  PARM_DESCRIPTOR *first;	/* first parm descriptor in list */
  PARM_DESCRIPTOR *last;	/* last parm descriptor in list */
} PARM_PROFILE;

typedef struct interface_scn {
  Elf_Ifd eid;	                /* described in cmplrs/elf_interfaces.h */
  PARM_PROFILE *parm;		/* optional parameter profile */
  ST *symbol;			/* compiler symbol table entry */
  struct interface_scn *next;	/* next interface descriptor */
} INTERFACE_SCN;

#define INTERFACE_SCN_eid(is)		((is).eid)
#define INTERFACE_SCN_parm(is)		((is).parm)
#define INTERFACE_SCN_symbol(is)	((is).symbol)
#define INTERFACE_SCN_next(is)		((is).next)
#define pINTERFACE_SCN_eid(pis)		((pis)->eid)
#define pINTERFACE_SCN_parm(pis)	((pis)->parm)
#define pINTERFACE_SCN_symbol(pis)	((pis)->symbol)
#define pINTERFACE_SCN_next(pis)	((pis)->next)

#define pPARM_PROFILE_var_size(ppp)     ((ppp)->var_size)
#define pPARM_PROFILE_parm_count(ppp)   ((ppp)->parm_count)
#define pPARM_PROFILE_result(ppp)       ((ppp)->result)
#define pPARM_PROFILE_first(ppp)        ((ppp)->first)
#define pPARM_PROFILE_last(ppp)         ((ppp)->last)

#define pPARM_DESCRIPTOR_flags(ppd)     ((ppd)->flags)
#define pPARM_DESCRIPTOR_qual_count(ppd) ((ppd)->qual_count)
#define pPARM_DESCRIPTOR_fundamental_type(ppd) ((ppd)->fundamental_type)
#define pPARM_DESCRIPTOR_qualifiers(ppd) ((ppd)->qualifiers)
#define pPARM_DESCRIPTOR_qualifier(ppd,n) (((ppd)->qualifiers)[(n)])
#define pPARM_DESCRIPTOR_size(ppd)      ((ppd)->size)
#define pPARM_DESCRIPTOR_next(ppd)      ((ppd)->next)

/* Local phase trace flags: */
static BOOL Trace_Iface   = FALSE;	/* Interface sections trace */

/* Hash table to hold entries for the entire source file */
#define IFACE_SCN_HASH_SIZE	(1013)
static INTERFACE_SCN *Iface_Scn_Hash_Table[IFACE_SCN_HASH_SIZE];
static INT32 Iface_Scn_Hash_Count;

/* ====================================================================
 *
 * Print_Elf_Interface_Descriptor
 *
 * Print out fields of the given Elf_Ifd to TFile.
 *
 * ====================================================================
 */

static void
Print_Elf_Interface_Descriptor( Elf_Ifd *eid )
{
  fprintf( TFile, "  symbol: %u\n", eid->symbol );
  fprintf( TFile, "  attrs : 0x%x", eid->attrs );
  if ( eid->attrs & SA_PROTOTYPED )   fprintf( TFile, " SA_PROTOTYPED");
  if ( eid->attrs & SA_VARARGS )      fprintf( TFile, " SA_VARARGS" );
  if ( eid->attrs & SA_PIC )          fprintf( TFile, " SA_PIC" );
  if ( eid->attrs & SA_DSO_ENTRY )    fprintf( TFile, " SA_DSO_ENTRY" );
  if ( eid->attrs & SA_ADDRESSED )    fprintf( TFile, " SA_ADDRESSED" );
  if ( eid->attrs & SA_FUNCTION )     fprintf( TFile, " SA_FUNCTION" );
  if ( eid->attrs & SA_NESTED )       fprintf( TFile, " SA_NESTED" );
  if ( eid->attrs & SA_IGNORE_ERROR ) fprintf( TFile, " SA_IGNORE_ERROR" );
  if ( eid->attrs & SA_DEFINITION )   fprintf( TFile, " SA_DEFINITION");
  if ( eid->attrs & SA_AT_FREE )      fprintf( TFile, " SA_AT_FREE" );
  if ( eid->attrs & SA_FREE_REGS )    fprintf( TFile, " SA_FREE_REGS" );
  if ( eid->attrs & SA_PARAMETERS )   fprintf( TFile, " SA_PARAMETERS");
  if ( eid->attrs & SA_ALTINTERFACE ) fprintf( TFile, " SA_ALTINTERFACE");

  fprintf( TFile, "\n" );
  fprintf( TFile, "  pcnt  : %d\n",   eid->pcnt );
  fprintf( TFile, "  fpmask: 0x%x\n", eid->fpmask );
}

/* ====================================================================
 *
 * Print_INTERFACE_SCN
 *
 * Print out fields of the given INTERFACE_SCN to TFile.
 *
 * ====================================================================
 */

static void
Print_INTERFACE_SCN( INTERFACE_SCN *iface_scn )
{
  Print_Elf_Interface_Descriptor( &(pINTERFACE_SCN_eid(iface_scn)) );
}


/* ====================================================================
 *
 * Hash_INTERFACE_SCN
 *
 * Calculate a hash value based upon the given iface section.
 *
 * ====================================================================
 */

static UINT32
Hash_INTERFACE_SCN( INTERFACE_SCN *iface_scn )
{
  return 
    (
    (UINT32)(INTPTR)pINTERFACE_SCN_symbol(iface_scn)	|
    (pINTERFACE_SCN_eid(iface_scn).attrs << 16) |
    (pINTERFACE_SCN_eid(iface_scn).pcnt << 8)	|
    (pINTERFACE_SCN_eid(iface_scn).fpmask)
    ) % IFACE_SCN_HASH_SIZE;
}


/* ====================================================================
 *
 * Initialize_Interface_Description
 *
 * Prepare the interface descriptor for later filling in.  Associate
 * the descriptor with the given symbol (which may be NULL), and
 * set attributes based upon the symbol.
 *
 * ====================================================================
 */

static void
Initialize_Interface_Description( 
  INTERFACE_SCN *desc,		/* modified based on inputs */
  ST *symbol )			/* symbol to associate with desc */
{
  bzero( desc, sizeof(*desc) );

  if ( symbol != NULL ) {
    TY_IDX sym_type = ST_pu_type(symbol);

    if ( TY_kind(sym_type) != KIND_FUNCTION ) {
      ErrMsg( EC_Inv_TY, Kind_Name(TY_kind(sym_type)),
	      "Initialize_Interface_Description" );
      return;
    }

    pINTERFACE_SCN_symbol(desc) = symbol;

    /* get the Elf symbol number associated with this symbol */
    pINTERFACE_SCN_eid(desc).symbol = EMT_Put_Elf_Symbol (symbol);

    if ( ST_addr_taken(symbol) )
      pINTERFACE_SCN_eid(desc).attrs |= SA_ADDRESSED;

    if ( TY_has_prototype(sym_type) )
      pINTERFACE_SCN_eid(desc).attrs |= SA_PROTOTYPED;

    if ( TY_is_varargs(sym_type) )
      pINTERFACE_SCN_eid(desc).attrs |= SA_VARARGS;
  }
}


/* ====================================================================
 *
 * Is_Duplicate_Interface_Description
 *
 * Determine if there is already an exact duplicate of this interface
 * descriptor in our internal structures.  Error on the side of
 * caution because it doesn't matter if there are duplicates.
 *
 * ====================================================================
 */

static BOOL
Is_Duplicate_Interface_Description( INTERFACE_SCN *desc )
{
  INT32 hashval = Hash_INTERFACE_SCN( desc );

  if ( Iface_Scn_Hash_Table[hashval] != NULL ) {
    INTERFACE_SCN *tmpiface;

    for ( tmpiface = Iface_Scn_Hash_Table[hashval];
	  tmpiface != NULL;
	  tmpiface = pINTERFACE_SCN_next(tmpiface) )
    {
      if ( bcmp(tmpiface,desc,sizeof(*desc)) == 0 )
	return ( TRUE );

      /* TODO: When we start adding parameters, we can do even
       * more checks to see if the lists are identical.
       */
    }
  }

  return ( FALSE );
}


/* ====================================================================
 *
 * Want_Interface_Section_For_Function - Determine if we want to
 * create an interface section for this function.
 *
 * ====================================================================
 */

static BOOL
Want_Interface_Section_For_Function( ST *function )
{
  /* Initially, do not support:
   * 1. indirect calls (we don't know what function we're calling)
   * 2. static functions (compiler should already flag errors)
   */

  if (  function == NULL || 
	ST_is_export_local(function))
  {
    return ( FALSE );
  }

  if (DEBUG_Full_Interface_Check) {
    if ( ST_export(function) == EXPORT_PROTECTED ||
	 ST_export(function) == EXPORT_PREEMPTIBLE ||   /* default */
	 ST_export(function) == EXPORT_OPTIONAL )
      return ( TRUE );
    else
      return ( FALSE );
  } else {
    return ( TRUE );
  }
}


/* ====================================================================
 *
 * Add_Interface_Description
 *
 * Copy and Add the interface descriptor to our internal structures for
 * later emission.
 *
 * ====================================================================
 */

static void
Add_Interface_Description( INTERFACE_SCN *desc )
{
  INTERFACE_SCN *newdesc;
  INT32 hashval;

  if ( Trace_Iface ) Print_INTERFACE_SCN( desc );

  /* allocate and copy it */
  newdesc = TYPE_SRC_ALLOC(INTERFACE_SCN);
  *newdesc = *desc;

  /* add it to our hash table */
  hashval = Hash_INTERFACE_SCN( newdesc );
  pINTERFACE_SCN_next(newdesc) = Iface_Scn_Hash_Table[hashval];
  Iface_Scn_Hash_Table[hashval] = newdesc;
  Iface_Scn_Hash_Count++;
}


/* ====================================================================
 *
 * Classify_Type
 *
 * Fill in parameter descriptor fields as appropriate for the specified
 * type and return the number of bytes required to store this
 * parameter descriptor in the .MIPS.interfaces section
 *
 * ====================================================================
 */

static mUINT8
Classify_Type(
  PARM_DESCRIPTOR *pd,
  TY_IDX arg_type,
  BOOL ign_cv) /* true if all const/volatile quals should be ignored */
{
  TY_IDX ty;
  mUINT8 ft;
  mUINT8 *quals;
  mUINT8 is_fundamental_type = 0;
  mUINT8 qualifier_overflow = 0;
  mUINT8 pointer_seen = 0;
  mUINT8 qcnt = 0;
  mUINT8 nbytes = 0;

  ty = arg_type;
  quals = pPARM_DESCRIPTOR_qualifiers(pd);

  /* drill down to fundamental type */

  /* The outermost const and volatile qualifiers on a parameter or
     result type are not meaningful outside the function definition.
     In fact, these qualifiers are stripped off during the generation
     of WHIRL PARM nodes.  As a result, unless steps are taken here
     to avoid it, interface data generated for defs (from symbol table
     TY nodes) and for refs (from WHIRL nodes) will differ.  This
     would prevent use of an interface matching algorithm which does
     simple binary comparisons of the parameter descriptors.

     Example: in foo(const int i), the 'const' applies only to
     assignments to i within the body of foo.  It places no
     restrictions on x in the call foo(x).

     Constness and volatility of parameters passed by reference _does_
     matter, so only the outermost const and volatile (up to the first
     pointer or the fundamental type) can be removed.

     Example: foo(const int * const i) is, from the caller's
     perspective, equivalent to foo(const int *i)

     Note: 'outermost' refers to the ordering of TY or WN nodes used
     to represent the type, and not to the C-style declaration

     Note: for functions returning results via the first parameter,
     all const and volatile parameters can be stripped from the
     result type.  Such parameters will always be large structures and
     not pointers.  The ign_cv parameter will be nonzero in this case.
  */

  while (!is_fundamental_type) {

#ifdef IFACE_SCN_FULL_DEBUG
    if (Trace_Iface)
      Print_TY(TFile, ty, 1);
#endif /* IFACE_SCN_FULL_DEBUG */

    if (TY_is_volatile(ty) && pointer_seen)
      quals[qcnt++] = MOD_volatile;
    
    if (qcnt == 16) {
      qualifier_overflow = 1;
      break;
    }
    
    if (TY_is_const(ty) && pointer_seen)
      quals[qcnt++] = MOD_const;

    if (qcnt == 16) {
      qualifier_overflow = 1;
      break;
    }
    
    switch (TY_kind(ty)) {

    case KIND_POINTER:
      quals[qcnt++] = MOD_pointer_to;
      ty = TY_pointed(ty);
      if (!ign_cv)
	pointer_seen = 1;
      break;

    case KIND_ARRAY:
      quals[qcnt++] = MOD_array_of;
      ty = TY_etype(ty);
      break;

    case KIND_FUNCTION:
      quals[qcnt++] = MOD_function;
      ty = TY_ret_type(ty);
      break;

    default:
      is_fundamental_type = 1;
      break;
    }

    if (qcnt == 16) {
      qualifier_overflow = 1;
      break;
    }

  }

#ifdef IFACE_SCN_FULL_DEBUG
    if (Trace_Iface)
      Print_TY(TFile, ty, 1);
#endif /* IFACE_SCN_FULL_DEBUG */

  if (qualifier_overflow) {     /* unable to fully represent type */
    pPARM_DESCRIPTOR_fundamental_type(pd) = FT_unknown;
    qcnt = 0;
  }

  pPARM_DESCRIPTOR_qual_count(pd) = qcnt;

  /* express fundamental type as parm descriptor */

  pPARM_DESCRIPTOR_size(pd) = TY_size(ty);

  switch (TY_kind(ty)) {
  case KIND_INVALID:
    pPARM_DESCRIPTOR_fundamental_type(pd) = FT_unknown;
    break;
  case KIND_SCALAR:
    switch(TY_mtype(ty)) {
    case MTYPE_I1:  ft = FT_signed_char;    break;
    case MTYPE_I2:  ft = FT_signed_short;   break;
    case MTYPE_I4:  ft = FT_signed_int32;   break;
    case MTYPE_I8:  ft = FT_signed_int64;   break;
    case MTYPE_U1:  ft = FT_unsigned_char;  break;
    case MTYPE_U2:  ft = FT_unsigned_short; break;
    case MTYPE_U4:  ft = FT_unsigned_int32; break;
    case MTYPE_U8:  ft = FT_unsigned_int64; break;
    case MTYPE_F4:  ft = FT_float32;        break;
    case MTYPE_F8:  ft = FT_float64;        break;
    case MTYPE_F10: ft = FT_float128;       break;
    case MTYPE_F16: ft = FT_float128;       break;
    case MTYPE_FQ:  ft = FT_float128;       break;
    case MTYPE_C4:  /* complex is sometimes of KIND_SCALAR */
    case MTYPE_C8:
    case MTYPE_C10:
    case MTYPE_CQ:
        switch(TY_size(ty)) {
        case 8:  ft = FT_complex64;   break;
        case 16: ft = FT_complex128;  break;
        case 32: ft = FT_complex256;  break;
	default: ft = FT_unknown;     break;
        }
	break;
    case MTYPE_V:   ft = FT_void;           break;
    default:        ft = FT_unknown;        break;
    }
    pPARM_DESCRIPTOR_fundamental_type(pd) = ft;
    break;
  case KIND_STRUCT:
    switch(TY_mtype(ty)) {
    case MTYPE_C4: ft = FT_complex64;  break;
    case MTYPE_C8: ft = FT_complex128; break;
    case MTYPE_CQ:
    case MTYPE_C10:
      ft = FT_complex256; break;
    default: 
      if (TY_is_union(ty))
	ft = FT_union;
      else
	ft = FT_struct;
      break;
    }      
    pPARM_DESCRIPTOR_fundamental_type(pd) = ft;
    /* MIPS.interfaces section allows 32-bit size; be allows 64-bit size! */
    /* The following assignment could lose information!                   */
    pPARM_DESCRIPTOR_size(pd) = TY_size(ty);
    if (ft==FT_struct || ft==FT_union)
      pPARM_DESCRIPTOR_flags(pd) |= PDMF_SIZE;
    break;
  case KIND_VOID:
    pPARM_DESCRIPTOR_fundamental_type(pd) = FT_void;
    break;
  }

  /* require 2 bytes for constant header information, possibly 4 bytes for
     a size specification, and 1 byte for each qualifier */

  nbytes = qcnt + ((pPARM_DESCRIPTOR_flags(pd) & PDMF_SIZE) ? 4 : 0) + 2;
  return nbytes;
}



/* ====================================================================
 *
 * Add_Parameter_Descriptor
 *
 * Add new parameter descriptor to current parameter profile.
 *
 * ====================================================================
 */

static void
Add_Parameter_Descriptor(
  PARM_PROFILE *current_profile,
  TY_IDX arg_type,
  BOOL ign_cv)  /* true if all const/volatile qualifiers should be ignored */
{
  PARM_DESCRIPTOR *pd;
  mUINT8 nbytes;

#ifdef IFACE_SCN_FULL_DEBUG
  if (Trace_Iface)
    fprintf(TFile, "\nPARAMETER:\n");
#endif /* IFACE_SCN_FULL_DEBUG */

  pd = TYPE_SRC_ALLOC(PARM_DESCRIPTOR);
  bzero(pd, sizeof(PARM_DESCRIPTOR));
  if (pPARM_PROFILE_first(current_profile) == NULL) {
    pPARM_PROFILE_first(current_profile) = pd;
    pPARM_PROFILE_last(current_profile) = pd;
  } else {
    pPARM_DESCRIPTOR_next(pPARM_PROFILE_last(current_profile)) = pd;
    pPARM_PROFILE_last(current_profile) = pd;
  }
  pPARM_PROFILE_parm_count(current_profile) += 1;
  nbytes = Classify_Type(pd, arg_type, ign_cv);
  pPARM_PROFILE_var_size(current_profile) += nbytes;
}



/* ====================================================================
 *
 * Add_Result_Descriptor
 *
 * Add new result descriptor to current parameter profile.
 *
 * ====================================================================
 */

static void
Add_Result_Descriptor(
  PARM_PROFILE *current_profile,
  TY_IDX ret_type)
{
  PARM_DESCRIPTOR *pd;
  mUINT8 nbytes;

  pd = TYPE_SRC_ALLOC(PARM_DESCRIPTOR);
  bzero(pd, sizeof(PARM_DESCRIPTOR));
  nbytes = Classify_Type(pd, ret_type, 0);
  pPARM_PROFILE_parm_count(current_profile) += 1;
  pPARM_PROFILE_var_size(current_profile) += nbytes;
  pPARM_PROFILE_result(current_profile) = pd;
}


/* ====================================================================
 *
 * Set_Parameter_Info
 *
 * Calculate some parameter information based upon the argument list.
 *
 * ====================================================================
 */

static void
Set_Parameter_Info(
  INTERFACE_SCN *iface_scn,	/* Set information in this */
  TY_IDX func_type,		/* TY of function */
  WN *rwn)			/* WN of function entry/call */
{
  PLOC ploc;
  mINT32 total_size;
  mUINT32 flt_regs = 0;
  TY_IDX ty;
  PARM_PROFILE *pp = NULL;
  OPERATOR opr = WN_operator(rwn);
  INT parm_count;
  INT i;
  BOOL func_entry;

#ifdef IFACE_SCN_FULL_DEBUG
  if (Trace_Iface) {
    Print_TY(TFile, func_type, 1);
    fprintf(TFile, "\nRETURN TYPE:\n\n");
  }
#endif /* IFACE_SCN_FULL_DEBUG */

  func_entry = opr == OPR_FUNC_ENTRY;

  ploc = func_entry ? Setup_Input_Parameter_Locations(func_type)
                    : Setup_Output_Parameter_Locations(func_type);

  pp = TYPE_SRC_ALLOC(PARM_PROFILE);
  bzero(pp, sizeof(PARM_PROFILE));

  /* Todo:  share this code with Calc_Actual_Area??? */

  parm_count = func_entry ? WN_num_formals(rwn) : WN_num_actuals(rwn);

  if (DEBUG_Full_Interface_Check) {
    if (TY_return_to_param(func_type))
      pPARM_PROFILE_result(pp) = NULL;  /* NULL => void */
    else
      Add_Result_Descriptor(pp, TY_ret_type(func_type));
  }
  for (i = 0; i < parm_count; i++) {
    ty = TY_Of_Parameter(WN_actual(rwn,i));
    if (DEBUG_Full_Interface_Check)
      Add_Parameter_Descriptor(pp, ty, (!i && TY_return_to_param(func_type)));
    if (func_entry) {
      ploc = Get_Input_Parameter_Location (ty);
      ploc = First_Input_PLOC_Reg (ploc, ty);
    } else {
      ploc = Get_Output_Parameter_Location (ty);
      ploc = First_Output_PLOC_Reg (ploc, ty);
    }
    // the int plocs are possibly adjusted by the register stack,
    // but we don't have the problem with the float regs.
    while (PLOC_is_nonempty(ploc)) {
    	if (Preg_Offset_Is_Float(PLOC_reg(ploc))) {
      		flt_regs |= (1 << (PLOC_reg(ploc) - Float_Preg_Min_Offset));
    	}
        ploc = func_entry ? Next_Input_PLOC_Reg (ploc)
                          : Next_Output_PLOC_Reg (ploc);
    }

  }
  
  if (DEBUG_Full_Interface_Check) {

    if ((pp->result != NULL) &&
	(pp->result->fundamental_type == FT_void) &&
	(pp->result->qual_count == 0)) {	
      /* no descriptor needs to be emitted for void return
       * types. Decrement parameter count, and reduce var_size to
       * reflect this fact */
      pPARM_PROFILE_result(pp) = NULL;
      pPARM_PROFILE_parm_count(pp) -= 1;
      pPARM_PROFILE_var_size(pp) -= 2;
    }
  
    if (pPARM_PROFILE_result(pp) == NULL) {
      pINTERFACE_SCN_eid(iface_scn).attrs &= ~SA_FUNCTION;
    } else {
      pINTERFACE_SCN_eid(iface_scn).attrs |= SA_FUNCTION;
    }

    /* remember that return type is included in parm_count (!) */
    parm_count = pPARM_PROFILE_parm_count(pp);
    pINTERFACE_SCN_eid(iface_scn).pcnt = parm_count;
    if (parm_count > 0)
      pINTERFACE_SCN_eid(iface_scn).attrs |= SA_PARAMETERS;
    
    pINTERFACE_SCN_parm(iface_scn) = pp;
    
  } else {  /* !DEBUG_Full_Interface_Check */

    /* HACK: Round the size up to the nearest 8-byte size because we're
     * interested in 64-bit chunks */
    total_size = 8*((PLOC_total_size(ploc)+7)/8);
    
    if ( Trace_Iface ) {
      fprintf ( TFile, "<iface_scn> total_size = %d, flt_regs = %x\n", 
		total_size, flt_regs);
    }
    
    /* HACK: for "compact" version, pcnt == # of 64-bit chunks.  This
     * "compact" version includes only the fixed-length Elf_Ifd
     * structure with the SA_PARAMETERS flag off and the pcnt field
     * set to reflect the number of register equivalents used to pass
     * parameters rather than the number of source-level parameters.
     * For more details refer to section 2.11.1 of the Elf-64 object
     * file specification */
    
    pINTERFACE_SCN_eid(iface_scn).pcnt = total_size/8;
    
  }

  /* SEMI-HACK: flt_regs now includes a mask of all of the flt. pt.
   * registers.  We want a mask of the argument registers, so shift
   * back so the first arg register ends up in bit 0 in the mask.
   */
  flt_regs = flt_regs >>
    (First_Float_Preg_Param_Offset - Float_Preg_Min_Offset);
  
  /* the fpmask field is just a byte, so mask off the least byte */
  pINTERFACE_SCN_eid(iface_scn).fpmask = flt_regs & 0xff;
  
}

/* ====================================================================
 *
 * Print_Interface
 *
 * Dump description of interface structures to TFile.
 *
 * ====================================================================
 */

#ifdef IFACE_SCN_DEBUG

static char *
Fundamental_Type_Name( UINT8 ft )
{
  char *r;

  switch (ft) {
  case FT_unknown: r = "FT_unknown"; break;
  case FT_signed_char: r = "FT_signed_char"; break;
  case FT_unsigned_char: r = "FT_unsigned_char"; break;
  case FT_signed_short: r = "FT_signed_short"; break;
  case FT_unsigned_short: r = "FT_unsigned_short"; break;
  case FT_signed_int32: r = "FT_signed_int32"; break;
  case FT_unsigned_int32: r = "FT_unsigned_int32"; break;
  case FT_signed_int64: r = "FT_signed_int64"; break;
  case FT_unsigned_int64: r = "FT_unsigned_int64"; break;
  case FT_pointer32: r = "FT_pointer32"; break;
  case FT_pointer64: r = "FT_pointer64"; break;
  case FT_float32: r = "FT_float32"; break;
  case FT_float64: r = "FT_float64"; break;
  case FT_float128: r = "FT_float128"; break;
  case FT_complex64: r = "FT_complex64"; break;
  case FT_complex128: r = "FT_complex128"; break;
  case FT_complex256: r = "FT_complex256"; break;
  case FT_void: r = "FT_void"; break;
  case FT_bool32: r = "FT_bool32"; break;
  case FT_bool64: r = "FT_bool64"; break;
  case FT_label32: r = "FT_label32"; break;
  case FT_label64: r = "FT_label64"; break;
  case FT_struct: r = "FT_struct"; break;
  case FT_union: r = "FT_union"; break;
  case FT_enum: r = "FT_enum"; break;
  case FT_typedef: r = "FT_typedef"; break;
  case FT_set: r = "FT_set"; break;
  case FT_range: r = "FT_range"; break;
  case FT_member_ptr: r = "FT_member_ptr"; break;
  case FT_virtual_ptr: r = "FT_virtual_ptr"; break;
  case FT_class: r = "FT_class"; break;
  default: r = "*unknown*"; break;
  }

  return r;
}


static char *
Qualifier_Name( UINT8 mod )
{
  char *r;

  switch (mod) {
  case MOD_pointer_to: r = "MOD_pointer_to"; break;
  case MOD_reference_to: r = "MOD_reference_to"; break;
  case MOD_const: r = "MOD_const"; break;
  case MOD_volatile: r = "MOD_volatile"; break;
  case MOD_function: r = "MOD_function"; break;
  case MOD_array_of: r = "MOD_array_of"; break;
  default: r = "*unknown*"; break;
  };

  return r;
}

static void
Print_Interface( INTERFACE_SCN *ifc )
{
  PARM_PROFILE *pp;
  PARM_DESCRIPTOR *pd;
  INT pcnt, qcnt;
  INT i, j;

  if (pINTERFACE_SCN_parm(ifc)) {
    pp = pINTERFACE_SCN_parm(ifc);
    pd = pPARM_PROFILE_result(pp);
    pcnt = pPARM_PROFILE_parm_count(pp);
    
    fprintf(TFile, "------------------------------------------------\n");
    fprintf(TFile, "variable size = %d\n", pPARM_PROFILE_var_size(pp));
    if (pd) {
      fprintf(TFile, "==> returns : %s(0x%02hx)  size=%u\n",
	      Fundamental_Type_Name(pPARM_DESCRIPTOR_fundamental_type(pd)),
	      pPARM_DESCRIPTOR_fundamental_type(pd),	      
	      pPARM_DESCRIPTOR_size(pd));
      qcnt = pPARM_DESCRIPTOR_qual_count(pd);
      for (j=0; j<qcnt; j++)
	fprintf(TFile, "==>     qualifier %d: %s\n", j+1,
		Qualifier_Name(pPARM_DESCRIPTOR_qualifier(pd,j)));
      fprintf(TFile, "==>     flags: 0x%02x\n", pPARM_DESCRIPTOR_flags(pd));
      --pcnt;
    } else {
      fprintf(TFile, "==> procedure\n");
    }
    
    pd = pPARM_PROFILE_first(pp);
    if (pd) {
      for (i=0; i<pcnt; i++) {
	fprintf(TFile, "==> %d : %s(0x%02hx)  size=%u\n", i+1,
		Fundamental_Type_Name(pPARM_DESCRIPTOR_fundamental_type(pd)),
		pPARM_DESCRIPTOR_fundamental_type(pd),
		pPARM_DESCRIPTOR_size(pd));
	qcnt = pPARM_DESCRIPTOR_qual_count(pd);
	for (j=0; j<qcnt; j++)
	  fprintf(TFile, "==>     qualifier %d: %s\n", j+1,
		  Qualifier_Name(pPARM_DESCRIPTOR_qualifier(pd,j)));
	fprintf(TFile, "==>     flags: 0x%02x\n", pPARM_DESCRIPTOR_flags(pd));
	pd = pPARM_DESCRIPTOR_next(pd);
      }
    }
    fprintf(TFile, "------------------------------------------------\n");
  }
}

#endif /* IFACE_SCN_DEBUG */


/* ====================================================================
 *
 * Update_Interface_Scn - Update the interface for any information
 * that may not have been available when it was added.
 *
 * ====================================================================
 */
static void
Update_Interface_Scn( INTERFACE_SCN *iface_scn )
{
  ST *sym = pINTERFACE_SCN_symbol(iface_scn);

  if ( sym != NULL ) {
Is_True((pINTERFACE_SCN_eid(iface_scn).symbol != 0), ("null elf index in Update_Interface_Scn"));
  }
}

/* ====================================================================
 *
 * Emit_Interface_Scn - Emit the interface and associated parameters
 *
 * ====================================================================
 */

static void
Emit_Interface_Scn( INTERFACE_SCN *iface_scn )
{
  static mUINT8 *idata, *idp;
  static mUINT32 idata_size = 0;
  mUINT16 pcnt = 0;
  mUINT16 variable_size = 0;
  mUINT16 interface_size = 8;  /* minimum of 8 bytes for constant region */
  mUINT8 need_auxiliary_pcnt = 0;
  mUINT8 need_free_reg_mask = 0;
  mUINT8 qcnt = 0;
  mUINT8 pad_bytes = 0;
  mUINT8 n;
  PARM_PROFILE *pp;
  PARM_DESCRIPTOR *pd;

  Is_True( iface_scn != NULL,("NULL iface_scn in Emit_Interface_Scn") );

  /* lay out variable interface data in memory as it will appear
     in the .interfaces section. */

  if (idata_size == 0) {
    idata_size = 1024;
    idata = TYPE_SRC_ALLOC_N(mUINT8, idata_size);
  }

  if ( pp = pINTERFACE_SCN_parm(iface_scn) ) {

    pcnt = pPARM_PROFILE_parm_count(pp);
    
    if (pINTERFACE_SCN_eid(iface_scn).attrs & (SA_PARAMETERS|SA_FREE_REGS)) {
      
      /* We need to emit a variable region */
      
      /* pPARM_PROFILE_var_size(pp) contains the combined size of the
	 parameter descriptors associated with this interface
	 specification.  We still need to account for the remaining
	 components of the variable region (free register mask,
	 auxiliary parameter count & variable region size) and the
	 constant section of the interface description as described in
	 Table 33 and Figure 2 of the Elf64 Object File Spec.  This
	 additional information may require as much as 16 additional
	 bytes of storage */
      
      variable_size = pPARM_PROFILE_var_size(pp);

      variable_size += 2;    /* storage for variable region size */
      
      if (pINTERFACE_SCN_eid(iface_scn).attrs & SA_FREE_REGS) {
	need_free_reg_mask = 1;
	variable_size += 4;  /* storage for free register mask */
      }
      
      if (pcnt >= 255) {
	need_auxiliary_pcnt = 1;
	variable_size += 2;  /* storage for auxiliary param count */
      }
      
      /* According to the Elf64 spec, the constant region of the
	 interface descriptor needs to be 32-bit aligned.  Since the
	 constant region is 8 bytes long, it is sufficient to guarantee
	 that the variable region is a multiple of 4 bytes in length */
      
      pad_bytes = (4 - (variable_size % 4)) % 4;

      variable_size += pad_bytes;
      
      interface_size += variable_size;
      
    }
    
  }

  if (idata_size < interface_size) {
    idata_size *= 2;
    idata = TYPE_SRC_ALLOC_N(mUINT8, idata_size);
  }
  
  idp = idata;

  /* emit constant region of interface specification */
    
  if (need_auxiliary_pcnt)
    pINTERFACE_SCN_eid(iface_scn).pcnt = 255;
  bcopy(&pINTERFACE_SCN_eid(iface_scn), idp, 8);
  idp += 8;
    

  if ( DEBUG_Full_Interface_Check ) {

    /* emit profile size */
    *(idp++) = variable_size >> 8; *(idp++) = variable_size & 0xFF;
    
    /* emit auxiliary parameter count if needed */
    if (need_auxiliary_pcnt) {
      *(idp++) = pcnt >> 8;
      *(idp++) = pcnt & 0xFF;      
    }
    
    /* emit free register mask if needed */
    if (need_free_reg_mask) {
      ErrMsg( EC_Unimplemented, "Emit_Interface_Scn: free register mask" );
    }
    
    /* emit return type descriptor if needed */
    if (pd = pPARM_PROFILE_result(pp)) {
      
      qcnt = pPARM_DESCRIPTOR_qual_count(pd);
      
      *(idp++) = (pPARM_DESCRIPTOR_flags(pd) | qcnt);
      *(idp++) = (pPARM_DESCRIPTOR_fundamental_type(pd));
      
      if (pPARM_DESCRIPTOR_flags(pd) & PDMF_SIZE) {
	bcopy(&pPARM_DESCRIPTOR_size(pd), idp, 4);
	idp += 4;
      }
      
      for (n = 0; n < qcnt; n++)
	*(idp++) = pPARM_DESCRIPTOR_qualifier(pd,n);
      
    }
    
    /* emit descriptors for parameters if needed */
    for (pd = pPARM_PROFILE_first(pp);
	 pd != 0;
	 pd = pPARM_DESCRIPTOR_next(pd)) {
      
      qcnt = pPARM_DESCRIPTOR_qual_count(pd);
      
      *(idp++) = (pPARM_DESCRIPTOR_flags(pd) | qcnt);
      *(idp++) = (pPARM_DESCRIPTOR_fundamental_type(pd));
      
      if (pPARM_DESCRIPTOR_flags(pd) & PDMF_SIZE) {
	bcopy(&pPARM_DESCRIPTOR_size(pd), idp, 4);
	idp += 4;
      }
      
      for (n = 0; n < qcnt; n++)
	*(idp++) = pPARM_DESCRIPTOR_qualifier(pd,n);
    }
    
    /* null out the padding bytes */
    for (n=0; n<pad_bytes; n++)
      *(idp++) = 0;
    
  }  /* end if (DEBUG_Full_Interface_Check) */

#ifdef IFACE_SCN_DEBUG
  if (Trace_Iface) {
    mUINT8 *d;
    fprintf(TFile, "%s: ", ST_name(pINTERFACE_SCN_symbol(iface_scn)));
    for (d=idata, n=0; n<interface_size; d++, n++)
      fprintf(TFile, "%02hx ", *d);
    putc('\n', TFile);
  }
#endif /* IFACE_SCN_DEBUG */
  
  Em_Add_New_Interface(interface_size, idata);
}


/* ====================================================================
 *
 * Sort_Interface_Scn - Sort the interfaces by Elf symbol number
 *
 * ====================================================================
 */

static INT32 
Cmp_Interface_Scn( const void *ptr1, const void *ptr2 )
{
  INTERFACE_SCN *iface1 = *(INTERFACE_SCN **)ptr1;
  INTERFACE_SCN *iface2 = *(INTERFACE_SCN **)ptr2;
  Elf64_Word i1 = pINTERFACE_SCN_eid(iface1).symbol;
  Elf64_Word i2 = pINTERFACE_SCN_eid(iface2).symbol;

  if ( i1 < i2 )
    return -1;
  else if ( i1 > i2 )
    return 1;
  else
    return 0;
}

static void
Sort_Interface_Scn( INTERFACE_SCN *sort_array[], INT32 count )
{
#ifdef IFACE_SCN_DEBUG
  if ( Trace_Iface ) {
    INT32 i;
    fprintf ( TFile, "<iface_scn> Sort %d elements before:\n", count );
    for ( i = 0; i < count; i++ ) {
      Print_INTERFACE_SCN( sort_array[i] );
      fprintf( TFile, "-------------\n" );
    }
  }
#endif /* IFACE_SCN_DEBUG */

  qsort( sort_array, count, sizeof(sort_array[0]), Cmp_Interface_Scn);

#ifdef IFACE_SCN_DEBUG
  if ( Trace_Iface ) {
    INT32 i;
    fprintf ( TFile, "<iface_scn> Sort %d elements after:\n", count );
    for ( i = 0; i < count; i++ ) {
      Print_INTERFACE_SCN( sort_array[i] );
      fprintf( TFile, "-------------\n" );
    }
  }
#endif /* IFACE_SCN_DEBUG */
}

/* ====================================================================
 *
 * Interface_Scn_Begin_File 
 *
 * Initialize anything for the upcoming source file
 *
 * ====================================================================
 */

extern void 
Interface_Scn_Begin_File( void )
{
  if ( (Trace_Iface = Get_Trace ( TP_EMIT, 32 )) ) {
    fprintf( TFile, "<iface_scn> Interface_Scn_Begin_File\n" );
  }

  /* init a hash table to hold the entries until we're done */
  bzero( Iface_Scn_Hash_Table, sizeof(Iface_Scn_Hash_Table) );
  Iface_Scn_Hash_Count = 0;
}


/* ====================================================================
 *
 * Interface_Scn_End_File 
 *
 * Finalize anything for the just-handled source file:
 *
 * o  Fill in any missing information that may have been finalized
 *    after the interface was added to our tables.
 * o  Sort the entries
 * o  Emit the entries
 *
 * ====================================================================
 */

extern void 
Interface_Scn_End_File( void )
{
  INTERFACE_SCN **sorting_array;
  INT32 hash_index, sort_index;

  if ( Trace_Iface ) {
    fprintf( TFile, "<iface_scn> Interface_Scn_End_File\n" );
  }

  if ( Iface_Scn_Hash_Count <= 0 ) return;

  sorting_array = 
    TYPE_SRC_ALLOC_N( INTERFACE_SCN *, Iface_Scn_Hash_Count );
  sort_index = 0;

  for ( hash_index = 0; hash_index < IFACE_SCN_HASH_SIZE; hash_index++ )
  {
    INTERFACE_SCN *tmp_iface;
    for ( tmp_iface = Iface_Scn_Hash_Table[hash_index];
	  tmp_iface != NULL;
	  tmp_iface = pINTERFACE_SCN_next(tmp_iface) )
    {
      Update_Interface_Scn( tmp_iface );
      Is_True( sort_index < Iface_Scn_Hash_Count,
	("Too many entries in Iface_Scn_Hash_Table") );
      sorting_array[sort_index++] = tmp_iface;
    }
  }

  Is_True( sort_index == Iface_Scn_Hash_Count,
    ("Mismatch between sort_index (%d), Iface_Scn_Hash_Count (%d)\n",
     sort_index, Iface_Scn_Hash_Count) );

  Sort_Interface_Scn( sorting_array, Iface_Scn_Hash_Count );

  for ( sort_index = 0; sort_index < Iface_Scn_Hash_Count; sort_index++)
  {
    Emit_Interface_Scn( sorting_array[sort_index] );
  }
}


/* ====================================================================
 *
 * Interface_Scn_Add_Def 
 *
 * Create an interface descriptor for the given subroutine entry point
 * which this entry block starts.
 *
 * ====================================================================
 */

extern void 
Interface_Scn_Add_Def( ST *entry_sym, WN *rwn)
{
  INTERFACE_SCN new_desc;

  /* Make sure we're declaring a function */
  if ( ST_class(entry_sym) != CLASS_FUNC ) {
    ErrMsg( EC_Unimplemented, "Interface_Scn_Add_Def: Not function" );
    return;
  }

  if ( Trace_Iface ) {
    fprintf( TFile, "<iface_scn> Interface_Scn_Add_Def: " );
    Print_ST ( TFile, entry_sym, FALSE );
  }

  /* do we really want to do anything with this symbol? */
  if ( ! Want_Interface_Section_For_Function( entry_sym ) ) {
    return;
  }

  /* create an interface descriptor for this function */
  Initialize_Interface_Description( &new_desc, entry_sym );

  /* Add definitions only for varargs functions unless the
   * user has specified -DEBUG:full_interface_check :
   */
  if ( DEBUG_Full_Interface_Check
    || ( INTERFACE_SCN_eid(new_desc).attrs & SA_VARARGS ))
  {
    /* This is a definition: */
    INTERFACE_SCN_eid(new_desc).attrs |= SA_DEFINITION;

#ifdef IFACE_SCN_DEBUG
    if (Trace_Iface) {
      fprintf(TFile, "\nDEFINITION: %s\n", ST_name(entry_sym));
      fprintf(TFile, "CLASS = %d  SCLASS = %d\n",
	      ST_symclass(entry_sym),
	      ST_sclass(entry_sym));
      fprintf(TFile, "FLAGS = %08x  FLAGS2 = %08x\n",
	      ST_flags(entry_sym),
	      STF_flags2(entry_sym));
    }
#endif /* IFACE_SCN_DEBUG */

    /* Process formal arguments: */
    Set_Parameter_Info( &new_desc, ST_pu_type(entry_sym), rwn);

#ifdef IFACE_SCN_DEBUG
    if (Trace_Iface)
      Print_Interface(&new_desc);
#endif /* IFACE_SCN_DEBUG */

    /* Copy the descriptor and add to our internal structure */
    Add_Interface_Description( &new_desc );
  }
}


/* ====================================================================
 *
 * Interface_Scn_Add_Call
 *
 * Create an interface descriptor for the given subroutine call
 *
 * ====================================================================
 */

extern void 
Interface_Scn_Add_Call( ST *call_sym, WN *call_wn)
{
  INTERFACE_SCN new_desc;

  if ( Trace_Iface ) {
    fprintf( TFile, "<iface_scn> Interface_Scn_Add_Call: " );
    Print_ST ( TFile, call_sym, FALSE );
  }

  /* do we really want to do anything with this symbol? */
  if ( ! Want_Interface_Section_For_Function( call_sym ) ) {
    return;
  }

  /* create an interface descriptor for this function */
  Initialize_Interface_Description( &new_desc, call_sym );

  /* Add call entries only for unprototyped routines unless the
   * user has specified -DEBUG:full_interface_check :
   */
  if ( DEBUG_Full_Interface_Check
    || (( INTERFACE_SCN_eid(new_desc).attrs & SA_PROTOTYPED ) == 0 ))
  {

#ifdef IFACE_SCN_DEBUG
    if (Trace_Iface) {
      fprintf(TFile, "\nCALL: %s\n", ST_name(call_sym));
      fprintf(TFile, "CLASS = %d  SCLASS = %d\n",
	      ST_symclass(call_sym),
	      ST_sclass(call_sym));
      fprintf(TFile, "FLAGS = %08x  FLAGS2 = %08x\n",
	      ST_flags(call_sym),
	      STF_flags2(call_sym));
    }
#endif /* IFACE_SCN_DEBUG */

    /* Process formal arguments: */
    Set_Parameter_Info( &new_desc, ST_pu_type(call_sym), call_wn);

#ifdef IFACE_SCN_DEBUG
    if (Trace_Iface)
      Print_Interface(&new_desc);
#endif /* IFACE_SCN_DEBUG */

    /* Generate interface entry only for routines with a FP parameter */
    /* unless DEBUG_Full_Interface_Check is set.                      */

    if ((INTERFACE_SCN_eid(new_desc).fpmask == 0) &&
	(! DEBUG_Full_Interface_Check))
    {
      return;
    }

    /* Add it if it's not a duplicate.  It's not absolutely necessary
     * to weed out duplicates, but it's more efficient.
     */
    if ( ! Is_Duplicate_Interface_Description( &new_desc ) ) {
      /* copy the descriptor and add to our internal structure */
      Add_Interface_Description( &new_desc );
    }
  }
}

#else /* ! defined(BUILD_OS_DARWIN) */
#include "wn.h"
extern void Interface_Scn_Begin_File( void ) {}
extern void Interface_Scn_End_File( void ) {}
extern void Interface_Scn_Add_Def( ST *, WN *) {}
extern void Interface_Scn_Add_Call( ST *, WN *) {}
#endif /* ! defined(BUILD_OS_DARWIN) */

