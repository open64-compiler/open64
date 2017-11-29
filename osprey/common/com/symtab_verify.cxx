/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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



#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop

//
// Symbol Table Verifiers; This file should be consistent with the
// Whirl Symbol Table specification.
// PLEASE DO NOT UPDATE this FILE w/o first checking with tk or wilson
// 


#ifndef symtab_INCLUDED
#include "symtab.h"                     // for Scope_tab
#endif

#include "symtab_defs.h"
#include "erglob.h"

// ======================================================================
// Auxiliary functions used by ST::Verify()
// ======================================================================
// TCON: verifier
// Can't define TCON::Verify as a member fn since TCON is still a "C" struct
// See Table 24

static void
TCON_Verify (TCON tc) {
#ifdef Is_True_On
  switch ( TCON_ty (tc)) {
  case MTYPE_STRING:
    Is_True( Targ_String_Address (tc) != NULL,
             ("Invalid TCON string pointer"));
    break;
  default:
    break;
  }
#endif // Is_True_On 
} // TCON_Verify

// ST_Verify_Class_Sclass verifies that the "symbol class" and "storage class" 
// of a symbol are mutually consistent. 
// (See table 6 in the Whirl Symbol Table Specification)
// Also called for partial verification

void 
ST_Verify_Class_Sclass(ST_CLASS sym_class, ST_SCLASS storage_class) {
    static char msg[] = "Invalid storage class (%s) for %s";
    switch (sym_class) {
    case CLASS_UNK:
	Is_True (storage_class == SCLASS_UNKNOWN,
		 (msg, Sclass_Name (storage_class), Class_Name (sym_class)));
	break;
    case CLASS_VAR:
	switch (storage_class) {
	case SCLASS_AUTO:
	case SCLASS_FORMAL:
	case SCLASS_FORMAL_REF:
	case SCLASS_PSTATIC:
	case SCLASS_FSTATIC:
	case SCLASS_COMMON:
	case SCLASS_EXTERN:
	case SCLASS_UGLOBAL:
	case SCLASS_DGLOBAL:
	case SCLASS_CPLINIT:
	case SCLASS_EH_REGION:
	case SCLASS_EH_REGION_SUPP:
	case SCLASS_DISTR_ARRAY:
	case SCLASS_THREAD_PRIVATE_FUNCS:
	    break;
	default:
	    Fail_FmtAssertion (msg, Sclass_Name (storage_class),
			       Class_Name (sym_class));
	    break;
	}
	break;
    case CLASS_FUNC:
	Is_True (storage_class == SCLASS_EXTERN ||
		 storage_class == SCLASS_TEXT,
		 (msg, Sclass_Name (storage_class), Class_Name (sym_class)));
	break;
    case CLASS_CONST:
	Is_True (storage_class == SCLASS_FSTATIC ||
		 storage_class == SCLASS_EXTERN,
		 (msg, Sclass_Name(storage_class), Class_Name(sym_class)));
	break;
    case CLASS_PREG:
	Is_True (storage_class == SCLASS_REG,
		 (msg, Sclass_Name(storage_class), Class_Name(sym_class)));
	break;
    case CLASS_BLOCK:
        Is_True (storage_class != SCLASS_REG,
		 (msg, Sclass_Name(storage_class), Class_Name(sym_class)));
	break;
    case CLASS_NAME:
         Is_True (storage_class == SCLASS_UNKNOWN ||
		  storage_class == SCLASS_COMMENT,
                  (msg, Sclass_Name(storage_class), Class_Name(sym_class)));
        break;
    default:
	Fail_FmtAssertion (msg, Sclass_Name (storage_class),
			   Class_Name (sym_class));
        break;
    }
}

//
// ST_Verify_SClass_Export verifies that the "storage class" and "Export scope"
// of a symbol are mutually consistent. 
// (See table 8 in the Whirl Symbol Table Specification)
// Also called for partial verification

void 
ST_Verify_Sclass_Export (ST_SCLASS storage_class, ST_EXPORT export_class,
			 const ST* st) 
{
    static char msg[] = "Invalid export scope (%s) for storage class (%s)";
    switch (storage_class) {
    case SCLASS_UNKNOWN:
    case SCLASS_AUTO:
    case SCLASS_FORMAL:
    case SCLASS_FORMAL_REF:
    case SCLASS_PSTATIC:
    case SCLASS_FSTATIC:
    case SCLASS_CPLINIT:
    case SCLASS_EH_REGION:
    case SCLASS_EH_REGION_SUPP:
    case SCLASS_DISTR_ARRAY:
    case SCLASS_THREAD_PRIVATE_FUNCS:
    case SCLASS_COMMENT:
      // bug fix for OSP_145, OSP_339, __attribute__((alias(...)))
      if ( export_class == EXPORT_PREEMPTIBLE ) {
        // maybe alias to FSTATIC
        ST_IDX base_idx = ST_base_idx (st);
        Is_True ( base_idx != ST_st_idx (st),
		  (msg, Export_Name(export_class), Sclass_Name (storage_class)));
        if (! (ST_sclass(St_Table[base_idx]) == SCLASS_UNKNOWN &&
               ST_class(St_Table[base_idx])  == CLASS_BLOCK) )
          Is_True ( storage_class == ST_sclass(St_Table[base_idx]),
                    (msg, Export_Name(export_class),
                     Sclass_Name (storage_class)) );
      }
      else {
        Is_True (export_class == EXPORT_LOCAL ||
                 export_class == EXPORT_LOCAL_INTERNAL,
		 (msg, Export_Name(export_class), Sclass_Name (storage_class)));
      }
      break;
    case SCLASS_COMMON:
    case SCLASS_DGLOBAL:
	if (export_class == EXPORT_LOCAL ||
            export_class == EXPORT_LOCAL_INTERNAL) {
	    Is_True (st != NULL && ST_base_idx (st) != ST_st_idx (st) &&
		     ST_sclass (St_Table[ST_st_idx (st)]) == storage_class,
		     (msg, Export_Name(export_class),
                      Sclass_Name (storage_class)));
	    break;
	}

	// else, fall through
	
    case SCLASS_EXTERN:
    case SCLASS_UGLOBAL:
    case SCLASS_TEXT:
	break;

    case SCLASS_REG:
      Is_True (export_class == EXPORT_LOCAL ||
               export_class == EXPORT_LOCAL_INTERNAL,
               (msg, Export_Name(export_class), Sclass_Name (storage_class)));
      break;
    default:
      Fail_FmtAssertion (msg, Export_Name (export_class),
                         Sclass_Name (storage_class));
      break;
    }
}

//
// ST_Verify_Flags verifies that the flags have been set only when they should
// have been (See table 9 in the Whirl Symbol Table Specification)

static void
ST_Verify_Flags (const ST &s)
{
  static char msg[] = "Invalid %s (%s) for ST Flag: (%s)";

  if (ST_is_weak_symbol (s)) {
    Is_True(ST_export(s) != EXPORT_LOCAL &&
            ST_export(s) != EXPORT_LOCAL_INTERNAL,
            (msg, "Export scope", Export_Name(ST_export(s)), "ST_WEAK_SYMBOL"));

    if (ST_base_idx (s) != ST_st_idx (s))
      Is_True (ST_sclass (s) == SCLASS_EXTERN ||
               ST_sclass (s) == SCLASS_TEXT ||
               ST_sclass (s) == SCLASS_DGLOBAL ||
               ST_sclass (s) == SCLASS_UGLOBAL,
	       (msg, "Storage class", Sclass_Name(ST_sclass(s)),
		"ST_WEAK_SYMBOL"));
  }
	       

  if (ST_is_initialized (s)) {
    Is_True(ST_sym_class(s) == CLASS_VAR || ST_sym_class(s) == CLASS_CONST ||
	    ST_sym_class(s) == CLASS_BLOCK,
            (msg, "Class", Class_Name(ST_sym_class(s)), "ST_INITIALIZED"));

    Is_True(ST_sclass(s) == SCLASS_PSTATIC ||
            ST_sclass(s) == SCLASS_FSTATIC ||
            ST_sclass(s) == SCLASS_EXTERN  ||
            ST_sclass(s) == SCLASS_DGLOBAL ||
            ST_sclass(s) == SCLASS_UGLOBAL ||
            ST_sclass(s) == SCLASS_CPLINIT ||
            ST_sclass(s) == SCLASS_EH_REGION ||
            ST_sclass(s) == SCLASS_EH_REGION_SUPP ||
            ST_sclass(s) == SCLASS_DISTR_ARRAY ||
            ST_sclass(s) == SCLASS_THREAD_PRIVATE_FUNCS ||
	    (ST_sclass(s) == SCLASS_UNKNOWN && ST_sym_class(s) == CLASS_BLOCK),
            (msg, "Storage class", Sclass_Name(ST_sclass(s)), "ST_INITIALIZED"));
    if (ST_sclass(s) == SCLASS_UGLOBAL)
      Is_True (ST_init_value_zero (s),
	       (msg, "Storage class", Sclass_Name(ST_sclass(s)),
		"ST_INIT_VALUE_ZERO (must be set)"));
  }

  // conversely ST_is_initialized better be set for SCLASS_DGLOBALS 
  if (ST_sclass(s) == SCLASS_DGLOBAL)
    Is_True(ST_is_initialized (s),
            (msg,"Storage class", Sclass_Name(SCLASS_DGLOBAL),
	     "ST_INITIALIZED (musr be set)"));

             
  if (ST_is_return_var (s)) 
    Is_True(ST_sclass(s) == SCLASS_AUTO ||
            ST_sclass(s) == SCLASS_PSTATIC || // -static
            ST_sclass(s) == SCLASS_FORMAL,    // functions returning character*
            (msg, "Storage class", Sclass_Name(ST_sclass(s)), 
	     "ST_IS_RETURN_VAR"));

  if (ST_is_value_parm (s))
    Is_True (ST_sclass (s) == SCLASS_FORMAL ||
	     ST_sclass (s) == SCLASS_FORMAL_REF,
	     (msg, "Storage class", Sclass_Name (ST_sclass (s)),
	      "ST_IS_VALUE_PARM"));

  if (ST_is_reshaped (s)) 
    Is_True(ST_sym_class(s) == CLASS_VAR,
            (msg, "Class", Class_Name(ST_sym_class(s)), "ST_IS_RESHAPED"));
  
  if (ST_emit_symbol (s)) 
    Is_True(ST_sym_class(s) == CLASS_NAME ||
	    ST_sym_class(s) == CLASS_FUNC ||
	    ST_sym_class(s) == CLASS_VAR,
            (msg, "Class", Class_Name(ST_sym_class(s)), "ST_EMIT_SYMBOL"));

  if (ST_has_nested_ref (s)) 
    Is_True(ST_sclass(s) == SCLASS_AUTO ||
            ST_sclass(s) == SCLASS_FORMAL ||
            ST_sclass(s) == SCLASS_FORMAL_REF ||
            ST_sclass(s) == SCLASS_PSTATIC,
            (msg, "Storage class", Sclass_Name(ST_sclass(s)), 
	     "ST_HAS_NESTED_REF"));

  if (ST_init_value_zero (s)) {
    Is_True(ST_sym_class(s) == CLASS_VAR,
            (msg, "Class", Class_Name(ST_sym_class(s)), "ST_INIT_VALUE_ZERO")); 

    Is_True(ST_sclass(s) == SCLASS_EXTERN ||
	    ST_sclass(s) == SCLASS_UGLOBAL ||
	    ST_sclass(s) == SCLASS_FSTATIC ||
	    ST_sclass(s) == SCLASS_PSTATIC,
	    (msg, "Storage class", Sclass_Name(ST_sclass(s)), 
	     "ST_INIT_VALUE_ZERO")); 

    Is_True(ST_is_initialized (s),
	    (msg, "ST_IS_INITIALIZED", Class_Name(ST_sym_class(s)),
	     "ST_INIT_VALUE_ZERO")); 

  }

  if (ST_gprel (s) || ST_not_gprel (s)) {
    Is_True(ST_sym_class(s) == CLASS_VAR ||
            ST_sym_class(s) == CLASS_CONST ||
	    ST_sym_class(s) == CLASS_BLOCK,
            (msg, "Class", Class_Name(ST_sym_class(s)), "ST_GPREL")); 

    Is_True(ST_sclass(s) != SCLASS_AUTO &&
	    ST_sclass(s) != SCLASS_FORMAL &&
	    ST_sclass(s) != SCLASS_FORMAL_REF, 
	(msg, "Storage class", Sclass_Name(ST_sclass(s)), "ST_GPREL")); 
  }

  if (ST_is_namelist (s)) 
    Is_True(ST_sym_class(s) == CLASS_VAR,
            (msg, "Class", Class_Name(ST_sym_class(s)), "ST_IS_NAMELIST")); 
    

  if (ST_is_f90_target (s)) 
    Is_True(ST_sym_class(s) == CLASS_VAR,
            (msg, "Class", Class_Name(ST_sym_class(s)), "ST_IS_F90_TARGET")); 
    

  if (ST_declared_static (s)) 
    Is_True(ST_sym_class(s) == CLASS_VAR,
            (msg, "Class", Class_Name(ST_sym_class(s)), "ST_DECLARED_STATIC")); 

  if (ST_is_equivalenced (s))
    Is_True(ST_sym_class(s) == CLASS_VAR,
            (msg, "Class", Class_Name(ST_sym_class(s)), "ST_IS_EQUIVALENCED")); 

  if (ST_is_fill_align (s))
    Is_True(ST_sym_class(s) == CLASS_VAR,
            (msg, "Class", Class_Name(ST_sym_class(s)), "ST_IS_FILL_ALIGN")); 

  if (ST_is_optional_argument (s))
    Is_True(ST_sclass(s) == SCLASS_FORMAL_REF ||
            ST_sclass(s) == SCLASS_FORMAL,
            (msg, "Storage class", Sclass_Name(ST_sclass(s)), 
	     "ST_IS_OPTIONAL_ARGUMENT")); 

  if (ST_is_temp_var (s))
    Is_True(ST_sclass(s) == SCLASS_AUTO ||
#if defined(TARG_NVISA)
	    // may treat auto as pstatic
            ST_sclass(s) == SCLASS_PSTATIC ||
#endif
            ST_sclass(s) == SCLASS_FORMAL ||
            ST_sclass(s) == SCLASS_FORMAL_REF,
            (msg, "Storage class", Sclass_Name(ST_sclass(s)), 
	     "ST_IS_TEMP_VAR")); 

  if (ST_is_const_var (s)) {
    Is_True(ST_sym_class(s) == CLASS_VAR,
            (msg, "Class", Class_Name(ST_sym_class(s)), "ST_IS_CONST_VAR")); 

    Is_True(ST_sclass(s) != SCLASS_AUTO   &&
#if !defined(TARG_NVISA)
            /* okay to have const formal in nvisa */
            ST_sclass(s) != SCLASS_FORMAL &&
#endif
            ST_sclass(s) != SCLASS_FORMAL_REF,
            (msg, "Storage class", Sclass_Name(ST_sclass(s)), 
	     "ST_IS_CONST_VAR")); 
  }

  if (ST_pt_to_unique_mem (s)) {
    Is_True (ST_sym_class (s) == CLASS_VAR,
	     (msg, "Class", Class_Name(ST_sym_class(s)),
	      "ST_PT_TO_UNIQUE_MEM"));
  }
   
  if (ST_addr_saved (s)) 
    Is_True(ST_sym_class(s) != CLASS_PREG,
	    (msg, "Class", Class_Name(ST_sym_class(s)), "ST_ADDR_SAVED"));
  
  if (ST_addr_passed (s)) 
    Is_True(ST_sym_class(s) != CLASS_PREG,
	    (msg, "Class", Class_Name(ST_sym_class(s)), "ST_ADDR_PASSED"));

  if (ST_is_shared_auto (s)) {
    Is_True(ST_sym_class(s) == CLASS_VAR,
            (msg, "Class", Class_Name(ST_sym_class(s)), "ST_IS_SHARED_AUTO"));

    Is_True(ST_sclass(s) == SCLASS_AUTO,
            (msg, "Storage class", Sclass_Name(ST_sclass(s)), 
	     "ST_IS_SHARED_AUTO")); 
  }

  Is_True (! (ST_is_weak_symbol (s) &&
              ST_is_split_common (s)),
	   (msg, "Flags", " ", "ST_is_split_common & ST_is_weak_symbol"));

}

// ======================================================================
// ST_Verify_Fields verifies  an ST's fields: See table 3: Layout of ST
// ======================================================================
static void
ST_Verify_Fields(const ST &s) 
{
  static char msg[] = "Invalid entry for ST Field: (%s)"; 
  if ( ST_sym_class(s) == CLASS_CONST) {
    Is_True( 0 < ST_tcon(s) && ST_tcon(s) < TCON_Table_Size (),
             (msg, "tcon"));

    // Verify the Tcon entry associated with this ST
    TCON_Verify(Tcon_Table[ST_tcon (s)]);
  }
  else {
    switch (ST_export (s)) {

    default:
      Is_True (0 < ST_name_idx (s), (msg, "name_idx"));
      // fall through

    case EXPORT_LOCAL:
    case EXPORT_LOCAL_INTERNAL:
      Is_True(ST_name_idx(s) < STR_Table_Size (), (msg, "name_idx"));
      break;
    }
  }

  if (ST_level (&s) > GLOBAL_SYMTAB)
    Is_True (ST_export (s) == EXPORT_LOCAL ||
	     ST_export (s) == EXPORT_LOCAL_INTERNAL,
	     (msg, "export"));

  switch (ST_sym_class (s)) {
  case CLASS_FUNC:
#ifndef KEY
    Is_True (ST_level (&s) == GLOBAL_SYMTAB, (msg, "sym_class"));
#endif
    Is_True( 0 < ST_pu(s) && ST_pu(s) < PU_Table_Size (), (msg, "pu")); 

    // Verify the PU associated with this st
    Pu_Table[ST_pu (s)].Verify (ST_level(&s));
    break;
  case CLASS_BLOCK:
    if (ST_blk(s) == 0)
	DevWarn ("ST_blk == 0?");
    else
	Is_True (ST_blk (s) > 0 && ST_blk (s) < Blk_Table.Size (),
             (msg, "name"));
    break;
  default:
  case CLASS_UNK:
    Fail_FmtAssertion (msg, Class_Name (ST_sym_class(s)));
    break;
  case CLASS_PREG:
    Is_True( ((&s == Return_Val_Preg) || (0 < TY_IDX_index (ST_type(s)) && 
             TY_IDX_index (ST_type(s)) < TY_Table_Size ())),
             (msg, "type")); 
    break;
  case CLASS_VAR:
  case CLASS_CONST:
    Is_True( 0 < TY_IDX_index (ST_type(s)) && 
             TY_IDX_index (ST_type(s)) < TY_Table_Size (),
             (msg, "type")); 
    break;
  case CLASS_NAME:
    // CLASS_NAME is set only for C++ symbols related to template proc
    Is_True((ST_sclass(s) == SCLASS_UNKNOWN ||
	     ST_sclass(s) == SCLASS_COMMENT) &&
            ST_export(s) == EXPORT_LOCAL,
            (msg,"sclass, export, base_idx"));

    Is_True( 0 < ST_name_idx(s) && ST_name_idx(s) < STR_Table_Size (),
            (msg, "name"));
    break;
  } // switch ST_sym_class (s)

  // EVERY ST should have base != 0

  Is_True (ST_base_idx(s) != 0, (msg, "base_idx"));

  if ( ST_base_idx(s) != ST_st_idx(s)) {
    // s is "based"; Let s be "based" on sb; ie s -> ... -> sb 
    // where ST_base_idx(sb) == ST_st_idx(sb)
    // verify s's offset and size are in sync with sb

    // This implies verifying following properties of s and sb;

    INT64 ofst;
    ST *sb;
    ST *snon_const = (ST *) &s;
    // casting away const is bAAAAD, but can't change Base_Symbol_And_Offset

    Base_Symbol_And_Offset ( snon_const, &sb, &ofst );

    // Property 1a.
    if ( ST_storage_class (*sb) != SCLASS_UNKNOWN) {

      if ( !ST_is_weak_symbol (s) )
        {
          /* open64.net bug878, aliased symbol storage class may not be equal 
             to the base. */
          if (ST_storage_class(s) != ST_storage_class(*sb)) {
            ErrMsg(EC_Sym_Sto_Cla, ST_name(s), ST_name(sb));
          }
        }
    }
    else {
      // Property 1b For blocks such as bss, SCLASS== UNKNOWN and EXPORT==LOCAL
      Is_True( ST_export(sb) == EXPORT_LOCAL && ST_class(sb) ==CLASS_BLOCK,
               (msg,"export scope, for based st with SCLASS_UNKNOWN should be EXPORT_LOCAL and class should be CLASS_BLOCK"));
    } 
    
    // Property 2: verify that ST_symclass of s and of sb are identical?
    if ( ST_sym_class (s) == CLASS_BLOCK) { 
      Is_True( ST_sym_class (s) == ST_sym_class (sb),
              (msg, "storage_class"));
    }

    // Property 3.
#ifdef TARG_MIPS
    // (Kannan) 
    // test = gcc.c-torture/compile/20000511-1.c
    // If argument list is longer that spans register and memory
    // then, this property may not be true.
    // So, need to exclude the test for SCLASS_FORMAL.
    if (ST_storage_class(s) != SCLASS_FORMAL)
#endif
    if (!(ofst + ST_size (&s) <= ST_size (sb)) && (ST_class(sb) != CLASS_BLOCK || !STB_is_basereg(sb)))
      DevWarn("ofst +  size of a ST (%s) should be less than size of its base (%s)", ST_name(s), ST_name(sb));

  } // Verification of properties of "based" sts
  else {
    // s is NOT based ie ST_base_idx(s) == ST_st_idx(s)
    Is_True(ST_ofst(s) == 0,
            ("offset: should be zero for st (%s) that is not based", ST_name(s)));
    }
} // ST_Verify_Fields

// ======================================================================
//  ST::Verify(): other st related checks go into this function
// ======================================================================
// ST::Verify Calls: TCON_Verify, PU::Verify, TY::Verify
void
ST::Verify (UINT) const
{
#ifdef Is_True_On
  ST_Verify_Class_Sclass (sym_class, storage_class);
  ST_Verify_Sclass_Export (storage_class,  export_class, this);
  ST_Verify_Flags(*this);
  ST_Verify_Fields(*this);
#endif // Is_True_On
} // ST::Verify


static BOOL
zero_dim_array(ST_IDX st_idx)
{ 
  ST& st = St_Table[st_idx];
  const TY& ty = Ty_Table[ST_type(st)];
  if (( TY_kind (ty) != KIND_ARRAY)
      ||   (ty.Arb() == 0)) 
    return FALSE;

  // Now check the bounds in each
  // dimension. 

  ARB_HANDLE arb(ty.Arb());
  INT i,ndim;
  ndim = ARB_dimension(arb);
  for (i = 0; i < ndim; i++) {
    if (ARB_const_lbnd (arb[i])
        && ARB_const_ubnd(arb[i]))
    { 
      if (ARB_lbnd_val(arb[i]) > ARB_ubnd_val(arb[i]))
	return TRUE;
    } 
  }
  return FALSE; 
}

// ======================================================================
//  INITO::Verify(): other INITO related checks go into this function
// ======================================================================
// (See table 25 and 26 in the Whirl Symbol Table Specification)



void INITO::Verify(UINT level) const 
{
#ifdef Is_True_On
  Is_True(ST_IDX_index (st_idx) > 0 &&
	  ST_IDX_index (st_idx) < ST_Table_Size (ST_IDX_level (st_idx)),
	  ("Invalid st_idx for INITO"));
#ifndef KEY // no longer true because INITO is used to link up nested functions
  Is_True(ST_is_initialized (St_Table[st_idx]),
	   ("ST_IS_INITIALIZED not set"));
#endif

  if (!zero_dim_array(st_idx))
  { 
    Is_True(0 <  val && val < INITV_Table_Size(),
             ("Invalid field for INITO: val"));
    Is_True(level == ST_IDX_level(st_idx),
	    ("INITO/st_idx level mismatch"));
  }
#endif
}

// ======================================================================
//  INITV::Verify(): other INITV related checks go into this function
// ======================================================================
// (See table 27 and 28 in the Whirl Symbol Table Specification)

void INITV::Verify(UINT) const
{
#ifdef Is_True_On
  static char msg[] = "Invalid entry for INITV Field: (%s)"; 

  switch (kind) {
  case INITVKIND_SYMOFF:
    Is_True ( repeat1 != 0,
              (msg, "repeat1: should not be 0"));
    break;
#ifdef TARG_IA64
  case INITVKIND_SYMIPLT:		// for function discriptor
    Is_True ( repeat1 != 0,
              (msg, "repeat1: should not be 0"));
    break;
#endif
  case INITVKIND_ZERO:
    Is_True ( repeat1 == 0,
              (msg, "repeat1: should be 0"));
    Is_True ( Repeat2 () != 0,
              (msg, "repeat2: should be 0"));
    break;
  case INITVKIND_ONE:
    Is_True ( repeat1 == 0,
              (msg, "repeat1: should be 0"));
    Is_True ( Repeat2 () != 0,
              (msg, "repeat2: should be 1"));
    break;
  case INITVKIND_VAL:
    Is_True ( repeat1 == 0,
              (msg, "repeat1: should be 0"));
    break;
  case INITVKIND_BLOCK:
#ifndef KEY // need blk == 0 for init of size 0 structs, as in bug 961
    Is_True ( Blk () != 0, (msg, "blk:  should not be 0"));
#endif // KEY
#ifdef KEY
    Is_True ( repeat1 != 0, (msg, "repeat1: should not be 0"));
    break;	// we are using 'unused' as flags
#endif // KEY
  case INITVKIND_LABEL:
    Is_True ( u.lab.flags >= INITVLABELFLAGS_FIRST && u.lab.flags <= INITVLABELFLAGS_LAST,
	     (msg, "bad label flag"));
    if ( u.lab.flags == INITVLABELFLAGS_UNUSED ) {
      Is_True ( u.lab.mtype == MTYPE_UNKNOWN, (msg, "bad mtype for lab") );
    }
    else {
      Is_True ( u.lab.mtype != MTYPE_UNKNOWN, (msg, "bad mtype for lab value") );
    }
    Is_True ( repeat1 != 0,
              (msg, "repeat1: should not be 0"));
    break;
  case INITVKIND_PAD:
    Is_True (u.pad.unused == 0,
	     (msg, "unused fillers for lab, blk, and  pad must be zero"));
    // fall through
  case INITVKIND_SYMDIFF:
  case INITVKIND_SYMDIFF16:
    Is_True ( repeat1 != 0,
              (msg, "repeat1: should not be 0"));
    break;
  case INITVKIND_UNK:
  default:
      Fail_FmtAssertion (msg, "kind: unknown");
    break;
  }
#endif
}

// ======================================================================
//  FLD::Verify(): other FLD related checks go into this function
// ======================================================================
// (See table 19 and 20 in the Whirl Symbol Table Specification)

void FLD::Verify (UINT64 record_size) const
{
#ifdef Is_True_On
  const  int FLD_BIT_FIELD_SIZE   = 64;
  const  int FLD_BIT_OFFSET_SIZE = 64;
  static char msg[] = "Invalid entry for FLD Field: (%s)"; 

  Is_True (name_idx < STR_Table_Size (), (msg, "name_idx"));

  Is_True (TY_IDX_index (type) < TY_Table_Size (), (msg, "type"));

  Is_True (bsize <= FLD_BIT_FIELD_SIZE, (msg, "bsize"));

  Is_True (bofst <= FLD_BIT_OFFSET_SIZE, (msg, "bofst"));

  if (ofst > record_size)
    Fail_FmtAssertion (msg, "ofst");

  else if (ofst == record_size) {

    if (flags & FLD_IS_BIT_FIELD) {

      // Do not give error for structs of the form
      //
      // struct example {
      //   unsigned int             i:1;
      //   unsigned int:            0;
      // } ex;

      Is_True (bsize == 0, (msg, "ofst"));
    } else {
      // handle zero-size types, such as array of zero length
      if ( !(this->flags & FLD_LAST_FIELD) || (TY_kind (this->type) != KIND_ARRAY))
        Is_True (TY_size (type) == 0, (msg, "ofst"));
    }
    
  }

  if (! (flags & FLD_IS_BIT_FIELD)) {
    Is_True ( bsize == 0, (msg, "bsize"));
    Is_True ( bofst == 0, (msg, "bofst"));
  }

  if (! (flags & FLD_EQUIVALENCE) && st) {
    Is_True (ST_IDX_level (st) == GLOBAL_SYMTAB, (msg, "st"));
  }


#endif // Is_True_On
} // FLD::Verify() 


static void
FLD_Verify_all (FLD_HANDLE fld_handle, UINT64 record_size) 
{
#ifdef Is_True_On
    FLD_ITER iter = Make_fld_iter (fld_handle);
    FLD_ITER last = Fld_Table.end ();

    do {
	(*iter).Verify (record_size);
    } while (!FLD_last_field(iter) && ++iter != last);

    Is_True (iter != last && FLD_last_field (iter),
	     ("Missing last field in FLD"));
#endif
} // FLD_Verify_all

// ======================================================================
//  ARB::Verify(): other ARB related checks go into this function
// ======================================================================
// (See table 22 and 23 in the Whirl Symbol Table Specification)

void ARB::Verify (mUINT16 dim) const
{
#ifdef Is_True_On
  static char msg[] = "Invalid entry for ARB Field: (%s)"; 

  Is_True (dimension == dim, (msg, "invalid array dimension"));

  Is_True (unused == 0, (msg, "Unused filler fields"));

  if (!(flags & ARB_CONST_LBND) && Lbnd_var() != 0) {
      Is_True (u1.var.unused == 0, (msg, "Unused filler fields"));
  }

  if ( ! (flags & ARB_CONST_UBND) && Ubnd_var() != 0) {
     Is_True( u2.var.unused == 0, (msg, "Unused filler fields"));
  }
  
  if ( ! (flags & ARB_CONST_STRIDE) && Stride_var() != 0) {
     Is_True( u3.var.unused == 0, (msg, "Unused filler fields"));
  }

  if (dim == 1)
      Is_True (flags & ARB_LAST_DIMEN, (msg, "missing ARB_LAST_DIMEN bit"));
  else
      Is_True (! (flags & ARB_LAST_DIMEN),
	       (msg, "ARB_LAST_DIMEN bit inconsistent with dimension"));

#endif // Is_True_On
} // ARB::Verify()

static void
ARB_Verify_all (ARB_HANDLE arb)
{
#ifdef Is_True_On
    ARB_IDX last = ARB_Table_Size ();

    Is_True (ARB_first_dimen (arb), ("Invalid ARB_IDX"));

    mUINT16 dim = ARB_dimension (arb);
    Is_True((dim>=1),("Invalid ARB dimension"));
    Is_True((arb.Idx() + dim <= last),("Invalid ARB_dimension"));
    Is_True(ARB_first_dimen(arb),("Missing ARB_FIRST_DIMEN bit"));
    for (INT i=0; i < dim; i++) {
       arb[i].Entry()->Verify(dim-i);
    }
#endif
} // ARB_Verify_all

// ======================================================================
//  LABEL::Verify(): other LABEL related checks go into this function
// ======================================================================
// (See table 29 and 30 in the Whirl Symbol Table Specification)

void LABEL::Verify(UINT) const
{
#ifdef Is_True_On
  Is_True( LABEL_name_idx (*this) < STR_Table_Size (),
           ("Invalid LABEL name"));
#endif // Is_True_On
} // LABEL::Verify() 

// ======================================================================
//  PREG::Verify(): other preg related checks go into this function
// ======================================================================
// (See table 31 in the Whirl Symbol Table Specification)

void PREG::Verify(UINT) const
{
#ifdef Is_True_On
  Is_True( PREG_name_idx (*this) < STR_Table_Size (),
           ("Invalid PREG name")); 
#endif // Is_True_On
} // PREG::Verify() 


// ======================================================================
//  ST_ATTR::Verify(): other st_attr related checks go into this function
// ======================================================================
void
ST_ATTR::Verify (UINT level) const
{
#ifdef Is_True_On
    Is_True(level == ST_IDX_level(st_idx),
	    ("ST_ATTR/st_idx level mismatch"));
    Is_True(ST_IDX_index (st_idx) > 0 &&
	    ST_IDX_index (st_idx) < ST_Table_Size (ST_IDX_level (st_idx)),
	    ("Invalid st_idx for ST_ATTR"));
    const ST& st = St_Table[st_idx];
    switch (kind) {
    case ST_ATTR_UNKNOWN:
	break;
    case ST_ATTR_DEDICATED_REGISTER:
	Is_True(ST_assigned_to_dedicated_preg (st),
		("ST_ASSIGNED_TO_DEDICATED_PREG not set"));
	Is_True(TY_is_volatile (ST_type (st)),
		("dedicated registers must be marked volatile"));
	break;
    case ST_ATTR_SECTION_NAME:
	Is_True(u.section_name < STR_Table_Size (), ("Invalid section name"));
	break;
    default:
	Fail_FmtAssertion ("Unknown ST_ATTR kind");
	break;
    }
#endif
}

// ======================================================================
// Auxiliary functions used by TY::Verify()
// ======================================================================
// (See table 18 in the Whirl Symbol Table Specification) 

void
TY_Verify_Kind_Mtype (TY_KIND kind, mTYPE_ID mtype)
{
  static char msg[] = "Invalid TY kind/mtype combination: %s";

  switch (kind) {
  case KIND_SCALAR:
      Is_True ( mtype != MTYPE_UNKNOWN &&
		mtype != MTYPE_V &&
		mtype != MTYPE_A4 &&
		mtype != MTYPE_A8,
		(msg, "For KIND_SCALAR, mtype cannot be VOID/UNKNOWN"));
      break;
  case KIND_ARRAY:
      Is_True ( mtype == MTYPE_UNKNOWN || mtype == MTYPE_M,
		(msg, "For KIND_ARRAY, mtype must be UNKNOWN"));
      break;
  case KIND_STRUCT:
      Is_True ( mtype == MTYPE_M, 
		(msg, "For KIND_STRUCT, mtype must be M"));
      break;
  case KIND_POINTER:
      {
	  TYPE_ID a_type = (Pointer_Size == MTYPE_byte_size (MTYPE_A4)) ?
	      MTYPE_A4 : MTYPE_A8;
	  Is_True ( mtype == Pointer_Mtype || mtype == a_type,
		    (msg, "For KIND_POINTER, mtype MUST be MTYPE_U4/U8/A4/A8"));
      }
      break;
  case KIND_FUNCTION:
      Is_True ( mtype == MTYPE_UNKNOWN,
		(msg, "For KIND_FUNCTION, mtype must be UNKNOWN"));
      break;
  case KIND_VOID:
      Is_True ( mtype == MTYPE_V,
		(msg, "For KIND_VOID, mtype must be VOID"));
      break;
  default:
      Fail_FmtAssertion (msg, "invalid KIND");
      break;
  }
}
// ======================================================================
//  TY::Verify(): other ty related checks go into this function
// ======================================================================
// (See table 13, 14, 15, 16, and 17 in the Whirl Symbol Table Specification)
// TY::Verify  Calls : FLD::Verify (if TY_kind is KIND_STRUCT/CLASS)
// also indirectly calls TY::Verify (if TY_kind is KIND_FUNCTION)

void TY::Verify(UINT) const
{
#ifdef Is_True_On
    static char msg[] = "Invalid TY entries: (%s)";
    FLD_IDX Fld_index;
    TYLIST_IDX Tylist_index;
    UINT32 ty_idx;

    TY_Verify_Kind_Mtype (kind, mtype);

    Is_True (name_idx < STR_Table_Size (), (msg, "name_idx out of bound"));

    switch (kind) {

    default:
	Fail_FmtAssertion (msg, "Invalid kind");
	break;

    case KIND_STRUCT:
	Fld_index = Fld ();

	Is_True (Fld_index < FLD_Table_Size (),
		 (msg, "TY::fld should be a valid index to the Fld_Table"));

	// Verify FLD
	if (Fld_index > 0)
	    FLD_Verify_all (FLD_HANDLE (Fld_index), size);

#ifndef KEY	// u2.copy_constructor
	Is_True (u2.etype == 0, (msg, "non-zero TY::etyp for KIND_STRUCT"));
#endif

	break;

    case KIND_FUNCTION:

	TY_Verify_Kind_Function (kind, size, mtype);

	Tylist_index = TY_tylist (*this);

	Is_True (Tylist_index > 0 && Tylist_index < TYLIST_Table_Size (),
		 (msg, "Invalid TY::tylist for KIND_FUNCTION"));

	ty_idx = TY_IDX_index (Tylist_Table[Tylist_index]);
	
	do {
	    Is_True (ty_idx > 0 && ty_idx < TY_Table_Size (),
		     (msg, "Invalid TY_IDX in prototype"));
	    ++Tylist_index;
	    Is_True (Tylist_index < TYLIST_Table_Size (),
		 (msg, "TYLIST_IDX out of bound"));
	    ty_idx = TY_IDX_index (Tylist_Table[Tylist_index]);
	} while (ty_idx != 0);
	
	break;

    case KIND_ARRAY:
	
	Is_True( TY_arb(*this).Idx() > 0 && TY_arb(*this).Idx() < ARB_Table_Size (),
		 (msg, "TY::arb should be set for KIND_ARRAY"));

	ARB_Verify_all (TY_arb (*this));

	Is_True( TY_IDX_index (TY_etype (*this)) > 0 &&
		 TY_IDX_index (TY_etype (*this)) < TY_Table_Size (),
		 (msg, "TY::etype should be set for KIND_ARRAY"));

	break;

    case KIND_VOID:
	
	Is_True (size == 0, (msg, "non-zero size for KIND_VOID"));

	Is_True (u1.fld == 0, (msg, "non-zero TY::fld for KIND_VOID"));
	
	Is_True (u2.etype == 0, (msg, "non-zero TY::etype for KIND_VOID"));

	break;

    case KIND_POINTER:
	Is_True (size == Pointer_Size, (msg, "Invalid size for KIND_POINTER"));

	Is_True (u1.fld == 0,
		 (msg, "non-zero TY::fld for KIND_POINTER"));

	Is_True (TY_IDX_index (TY_pointed (*this)) > 0 &&
		 TY_IDX_index (TY_pointed (*this)) < TY_Table_Size (),
		 (msg, "invalid TY_IDX for KIND_POINTER"));

	break;

    case KIND_SCALAR:

	Is_True (u1.fld == 0, (msg, "non-zero TY::fld for KIND_SCALAR"));

	Is_True (u2.etype == 0, (msg, "non-zero TY::etype for KIND_SCALAR"));

	break;
    }
    
    // Flag checking

    if ( TY_is_union (*this))
	Is_True (kind == KIND_STRUCT,
		 (msg, "TY_IS_UNION can only be set for STRUCTS"));

    if ( TY_is_packed (*this))
	Is_True (kind == KIND_STRUCT,
		 (msg, "TY_IS_PACKED can only be set for STRUCTS/CLASSES"));

    if ( TY_ptr_as_array (*this))
	Is_True (kind == KIND_POINTER,
		 (msg, "TY_PTR_AS_ARRAY can only be set for POINTERS")); 

    if ( TY_anonymous (*this))
	Is_True (kind == KIND_STRUCT || kind == KIND_ARRAY,
		 (msg, "TY_ANONYMOUS can only be set for STRUCTS/CLASSES/ARRAYS")); 

#endif // Is_True_On
} // TY::Verify() 

// ======================================================================
//  PU::Verify(): other pu related checks go into this function
// ======================================================================
// (See table 11 in the Whirl Symbol Table Specification)
// PU::Verify Calls : TY::Verify (to verify the prototype ty)

void PU::Verify(UINT) const
{
#ifdef Is_True_On

  // until we define what this is, it has to be zero
  Is_True (PU_target_idx (*this) == TARGET_INFO_IDX_ZERO,
	   ("PU::target_info_idx != 0"));

  // Verify prototype ty
  Is_True (TY_IDX_index (prototype) > 0 &&
	   TY_IDX_index (prototype) < TY_Table_Size (),
	   ("Invalid TY_IDX in PU::prototype"));

#ifdef KEY
// We are using 'misc' to store ST_IDXs of 2 special variables for
// C++ exception handling, or for C nested functions.
  if (!(src_lang & PU_CXX_LANG) && !(src_lang & PU_C_LANG))
    Is_True (misc == 0, ("misc fields must be zero"));
#endif // KEY

  Is_True (unused == 0, ("unused fields must be zero"));

  // Verify flags
  static char msg[] = "Invalid PU flags: (%s)";

  if ( PU_is_mainpu (*this) || 
       PU_has_return_address (*this))
    Is_True( PU_no_inline (*this),
            (msg, "Recursive or main pus and pus that contain return address, should be marked no-inline"));

  if ( PU_no_inline (*this) && PU_must_inline (*this))
    Fail_FmtAssertion (msg, "must_inline and no_inline");

  if ( PU_has_exc_scopes (*this))
    Is_True( PU_cxx_lang (*this),
            (msg, "exception scopes can only be set for a C++ language pu"));

  Is_True (PU_lexical_level (*this) > 1,
	   (msg, "Lexical level for pu should be > 1"));

  if ( PU_is_nested_func (*this))
    Is_True( PU_lexical_level (*this) > 2,
            (msg, "Lexical level for nested pu should be > 2"));

  if ( PU_args_aliased (*this))
    Is_True(PU_ftn_lang(*this),
            (msg, "PU_ARGS_ALIASED may only be set for F77 or F90"));

  if ( PU_has_namelist  (*this))
    Is_True(PU_ftn_lang(*this),
            (msg, "PU_HAS_NAMELIST may only be set for F77 or F90"));
    
  if ( PU_has_altentry (*this))
    Is_True(PU_ftn_lang(*this),
            (msg, "PU_HAS_ALTENTRY may only be set for F77 or F90"));

  if ( (PU_c_lang (*this) && PU_ftn_lang (*this)) ||
       (PU_cxx_lang (*this) && PU_ftn_lang (*this)) ||
       (PU_c_lang (*this) && PU_cxx_lang (*this)) ||
       (PU_f77_lang (*this) && PU_f90_lang (*this)))
    Fail_FmtAssertion (msg," PU src lang bits cannot be set to multiple languagess");

#endif // Is_True_On
} // PU::Verify()



// ======================================================================
//  FILE_INFO::Verify(): other file related checks go into this function
// ======================================================================
// (See table 32 and 33 in the Whirl Symbol Table Specification)

void FILE_INFO::Verify() const
{
#ifdef Is_True_On
  Is_True(unused == 0,
          ("Invalid FILE_INFO field: unused"));
#endif // Is_True_On
} // FILE_INFO::Verify() 


// ======================================================================
//  SCOPE::Verify(): other file related checks go into this function
// ======================================================================
// (See table 1 in the Whirl Symbol Table Specification)

template <class T>
struct verify_op
{
  UINT level;
  verify_op(UINT lev) : level(lev) { }
  void operator () (UINT idx, T *entry) const;
}; 

template <class T>
inline void 
verify_op<T>::operator () (UINT, T *entry) const {
    entry->Verify(level);
}

// specialization for verifying TCONs

template<>
inline void
verify_op <TCON>::operator () (UINT, TCON *tc) const 
{
    TCON_Verify(*tc);
}


// ======================================================================
//  Verify_LOCAL_SYMTAB(): 
// ======================================================================

// At the least verify ST, LABEL, PREG, INITO, and ST_ATTR: which are
// present in LOCAL AND GLOBAL symbol tables
// Shouldn't the second parameter type be UINT32 instead of
// SYMTAB_IDX? Levels are UINT32 everywhere else, it seems. -- RK
void  Verify_LOCAL_SYMTAB (const SCOPE& scope, SYMTAB_IDX level)
{
#ifdef Is_True_On

  // Verify st for the scope

  scope.st->Verify(level);

  // Verify TABLES: ST, LABEL, PREG, INITO

  if (ST_Table_Size (level)) {
    For_all_entries(*scope.st_tab      , verify_op<ST   >(level), 1);
  }
  if (LABEL_Table_Size (level)) {
    For_all_entries(*scope.label_tab   , verify_op<LABEL>(level), 1);
  }
  if (PREG_Table_Size (level)) {
    For_all_entries(*scope.preg_tab    , verify_op<PREG >(level), 1);
  }
  if (INITO_Table_Size (level)) {
    For_all_entries(*scope.inito_tab   , verify_op<INITO>(level), 1);
  }
  if (ST_ATTR_Table_Size (level)) {
    For_all_entries(*scope.st_attr_tab , verify_op<ST_ATTR >(level), 1);
  }

#endif // Is_True_On
}

// ======================================================================
//  Verify_GLOBAL_SYMTAB(): 
// ======================================================================

void  Verify_GLOBAL_SYMTAB()
{
  const SCOPE & scope = Scope_tab[GLOBAL_SYMTAB];

#ifdef Is_True_On  
  // Verify that LABEL and PREG tables are EMPTY
  Is_True ( scope.label_tab == NULL,
            ("LABEL Table can only appear in LOCAL symtab"));

  Is_True ( scope.preg_tab == NULL,
            ("PREG Table can only appear in LOCAL symtab"));

  // Verify TABLES: ST and INITO (LABEL, PREG, are empty)
  if ( ST_Table_Size (GLOBAL_SYMTAB))
    For_all (St_Table, GLOBAL_SYMTAB, verify_op<ST>(GLOBAL_SYMTAB));

  if ( INITO_Table_Size (GLOBAL_SYMTAB))
    For_all (Inito_Table, GLOBAL_SYMTAB, verify_op<INITO>(GLOBAL_SYMTAB));

  if ( ST_ATTR_Table_Size (GLOBAL_SYMTAB))
    For_all (St_Attr_Table, GLOBAL_SYMTAB, verify_op<ST_ATTR>(GLOBAL_SYMTAB));

  // Verify other tables
  // FILE_INFO, TY, FLD, ARB, TCON, 

  File_info.Verify();

  if ( TY_Table_Size () > 1)
    For_all (Ty_Table, verify_op<TY>(GLOBAL_SYMTAB));
  if ( TCON_Table_Size () )
    For_all (Tcon_Table, verify_op<TCON>(GLOBAL_SYMTAB));
  if ( INITV_Table_Size () > 1)
    For_all (Initv_Table, verify_op<INITV>(GLOBAL_SYMTAB));

#endif  // Is_True_On
}

