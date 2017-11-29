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


#ifndef stab_attr_INCLUDED
#define stab_attr_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: stab_attr.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:59-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.stab_attr.h $
 *
 * Revision history:
 *  07-Mar-95 - Original Version
 *
 * Description:
 *
 * Get TY and ST information, beyond that provided through 
 * common/com/stab.h.  Also, provide functions to access
 * and create identifier name attributes.
 *
 * Symbol table information
 * ------------------------
 *
 *    FILE_SCOPE_ID:
 *       The scope_id given to TY and ST entries that are declared
 *       with file-scope.
 *
 * Reserved names
 * --------------
 *
 *    Stab_Reserved_Ty:
 *       TRUE when the given type is from a system file which will
 *       be included anyway (e.g. through whirl2c.h).  Do not declare
 *       symbols for which this condition is TRUE, since this will
 *       lead to non-compilable whirl2c/whirl2f output!
 *
 *    Stab_Reserved_St:
 *       TRUE when the given symbol is from a system file which will
 *       be included anyway (e.g. through whirl2c.h).  Do not declare
 *       symbols for which this condition is TRUE, since this will
 *       lead to non-compilable whirl2c/whirl2f output!
 *
 * Flag indicating referenced symbols
 * ----------------------------------
 *
 *    Stab_Reset_Referenced_Flag:
 *       Will reset this flag for FOR_ALL_LOCAL_SYMBOLS and
 *       FOR_ALL_CONSTANTS in the given symbol table.
 *
 * Type information
 * ----------------
 *
 *    MTYPE:
 *       The name "MTYPE" is more intuitive than TYPE_ID as a
 *       type for variables that hold elements from the MTYPE
 *       enumeration.
 *
 *    Stab_Identical_Types:
 *       Two types are only identical if they have the same qualifiers,
 *       compatible kinds, compatible MTYPEs, and identical substructure.
 *       ENUM types are treated as scalars.  While constructed types must
 *       have identical substructure, we allow more lenient checks for
 *       the top-level types:  We can turn off qualifier checks 
 *       (check_quals == FALSE); we can treat all scalar values as 
 *       identical (check_scalars == FALSE); and we can treat pointers
 *       as scalars (ptrs_as_scalars == TRUE).
 *
 *    Stab_Assignment_Compatible_Types:
 *       Similar to Stab_identical_types, but with the added leniency
 *       for pointer types (i.e. qualifiers of pointed types)
 *       specified in the ANSI/ISO C standard.
 *
 *    Stab_Is_Element_Type_Of_Array:
 *       True if the given etype is an element of the given a(rray)
 *       type, or if it is an element of array elements of the array
 *       type.
 *
 *    Stab_Array_Has_Dynamic_Bounds:
 *       This routine takes an array type [TODO: handle pointers
 *       as arrays at every call site], and returns TRUE if all
 *       bounds and stride information is given in terms of constant
 *       values; otherwise FALSE will be returned.
 *
 *    Stab_Get_Mload_Ty:
 *       We have very limited information about MLOAD types, since we
 *       only keep the base address in the WN node.  This returns the
 *       type of the first field in the given struct/union "base" struct
 *       at the given offset and of the given (non-zero) size.
 *
 *    Stab_Mtype_To_Ty:
 *       Get the TY of an mtype.
 *
 *    TY_IS_xxxxx:
 *       Inquiries about what kind of type (TY) we are dealing with.
 *
 * Function types
 * --------------
 *
 *    Func_Return_Type:
 *       The return type for a given function-type.  Will be a
 *       Void_Type for whirl2c when Func_Return_Character==TRUE,
 *       since in C we return CHARACTER strings through an explicit
 *       first parameter.
 *       
 *    Func_Return_Character:
 *       TRUE when the given function type returns a Fortran CHARACTER
 *       string.  Can only return TRUE in Fortran mode.
 *
 *    Func_Return_To_Param:
 *       TRUE when the given function type returns a value into the
 *       location denoted by the first parameter.
 *
 * Type creation
 * -------------
 *    Somtimes it may be necessary to create new TY entries beyond
 *    those in the symbol-table generated by the compiler-phases,
 *    and for these occations we provide the following facilities.
 *    After processing of a given symbol table, we must reset any 
 *    TY fields to what they were before this translation.
 *
 *    Stab_Pointer_To:
 *       If the given TY has a TY_pointer attribute, then simply
 *       return it; otherwise, create a new TY entry and set the
 *       TY_pointer attribute of the pointee to point to it.
 *
 *    Stab_Array_Of:
 *       Creates a unique one-dimensional array type with the given
 *       number of elements and the given element type.
 *
 *    Stab_initialize:
 *       Record the size of certain tables in the global symtab.  This
 *       information is then used later on in function Stab_finalize
 *       to reset the tables back to their original size and, thus,
 *       undo any additions made to these tables during whirl2c.
 *
 *    Stab_finalize:
 *       Reset certain tables in the global symtab back to their size
 *       at the start of whirl2c, thereby undoing any additions made
 *       to these tables during whirl2c.
 *
 *
 * Identifier naming utilities
 * ---------------------------
 *    We operate with a cyclic character buffer for identifier names,
 *    where the size of the buffer is a minimum of 1024 characters
 *    and at a maximum of 8 times the largest name encountered.  Note 
 *    that a call to any of the functions described below may allocate
 *    a new name buffer.  Name buffers are allocated from the cyclic
 *    character buffer, and a name-buffer may be reused at every 8th
 *    allocation.  We guarantee that a name-buffer is valid up until
 *    7 subsequent name-buffer allocations, but no longer.  After
 *    7 subsequent name-buffer allocations, the name buffer may be
 *    reused (overwritten) or even freed up from dynamic memory.  While
 *    the results from the calls to the functions provided here may be
 *    used to construct identifier names, these results should be saved
 *    off into a more permanent buffer area once the names have been
 *    constructed.
 *
 *    Stab_Free_Namebufs:
 *       Frees up the memory allocated for string buffers.  This should
 *       be done between processing individual PUs.
 *
 *    Get_Name_Buf_Slot:
 *       Allocate a new name-buffer slot of the given size.  It is up
 *       to the caller to ensure that no reference is made to 
 *       characters outside the allocated slot.  The cyclic name
 *       buffer will become at least 8 times the size of this slot,
 *       so do not use this as a method for obtaining an arbitrary 
 *       temporary memory-pool!
 *
 *    Number_as_String:
 *       Convert the number into an equivalent ASCII character 
 *       string representation, using the given <stdio.h> formatting.
 *       A maximum of 128 characters will be allocated to hold the
 *       resultant string value.  Note that the format must accept
 *       a 64 bits integral value.
 *
 *    Ptr_as_String:
 *       Convert the pointer into an equivalent ASCII character 
 *       string representation. A maximum of 128 characters will
 *       be allocated to hold the resultant string value.
 *
 *    Concat2_Strings/Concat3_Strings:
 *       Concatenate two and three strings, respectively, into
 *       a new combined character string.
 *
 *    Name_Hash_Idx:
 *       Using the hash-value returned from Get_Hash_Value_For_Name,
 *       this gets an index into the hash-table based on the log(2, 
 *       tbl_size) rightmost characters of the name.  For a 
 *       tbl_size of 173, the rightmost 2 characters determines 
 *       the hash_idx.  For an empty ('\0' or NULL) name, the 
 *       hash value and index will be zero
 *
 * Temporary variable information
 * ------------------------------
 *    Stab_Free_Tmpvars:
 *       This will free up memory associated with the maintenance of
 *       temporary variables, and should be called after processing each
 *       PU.
 *
 *    Stab_Lock_Tmpvar:
 *       Return an identifying index for a tempvar of the given type.
 *       If none existed before-hand, then a new one will be declared
 *       locally to the current PU, using the subroutine provided, which
 *       declares the temporary variable based on its type and index.
 *
 *    Stab_Unlock_Tmpvar:
 *       Given a tmpvar identifying index, this tmpvar will now become
 *       available for other uses.
 *
 * ====================================================================
 * ====================================================================
 */

                     /*------ Type Information ------*/
                     /*------------------------------*/

typedef TYPE_ID MTYPE;

extern BOOL Stab_Identical_Types(TY_IDX t1, 
				 TY_IDX t2, 
				 BOOL   check_quals, 
				 BOOL   check_scalars,
				 BOOL   ptrs_as_scalars);

extern BOOL Stab_Assignment_Compatible_Types(TY_IDX t1, 
					     TY_IDX t2, 
					     BOOL   check_quals, 
					     BOOL   check_scalars,
					     BOOL   ptrs_as_scalars);

extern BOOL Stab_Is_Element_Type_Of_Array(TY_IDX atype, TY_IDX etype);

extern BOOL Stab_Array_Has_Dynamic_Bounds(TY_IDX ty);
extern BOOL Stab_Is_Assumed_Sized_Array(TY_IDX ty);
extern BOOL Stab_Is_Equivalenced_Struct(TY_IDX ty);

extern TY_IDX Stab_Get_Mload_Ty(TY_IDX base,
                                STAB_OFFSET offset,
                                STAB_OFFSET size);

inline TY_IDX Stab_Mtype_To_Ty(TYPE_ID mtype) { return Be_Type_Tbl(mtype); }

inline BOOL TY_Is_Pointer(TY_IDX ty)
{
   return TY_kind(ty) == KIND_POINTER;
} /* TY_Is_Pointer */

inline BOOL TY_Is_Array(TY_IDX ty)
{
   return TY_kind(ty) == KIND_ARRAY;
} /* TY_Is_Array */

inline BOOL TY_Is_Function(TY_IDX ty)
{
   return TY_kind(ty) == KIND_FUNCTION;
} /* TY_Is_Function */

inline BOOL TY_Is_Struct(TY_IDX ty)
{
   return TY_kind(ty) == KIND_STRUCT && !TY_is_union(ty);
} /* TY_Is_Struct */

inline BOOL TY_Is_Union(TY_IDX ty)
{
   return TY_kind(ty) == KIND_STRUCT && TY_is_union(ty);
} /* TY_Is_Union */

inline BOOL TY_Is_Structured(TY_IDX ty)
{
   return TY_kind(ty) == KIND_STRUCT;
} /* TY_Is_Structured */

inline BOOL TY_Is_String(TY_IDX ty)
{
   return TY_mtype(ty) == MTYPE_STRING;
} /* TY_Is_String */

inline BOOL TY_Is_Integral(TY_IDX ty)
{
   return (MTYPE_type_class(TY_mtype(ty)) & MTYPE_CLASS_INTEGER) != 0;
} /* TY_Is_Integral */

inline BOOL TY_Is_Quad(TY_IDX ty)
{
   return TY_kind(ty) == KIND_SCALAR && TY_mtype(ty) == MTYPE_FQ;
} /* TY_Is_Quad */

inline BOOL TY_Is_Complex(TY_IDX ty)
{
   return TY_kind(ty) == KIND_SCALAR && (TY_mtype(ty) == MTYPE_C4 ||
                                         TY_mtype(ty) == MTYPE_C8 ||
                                         TY_mtype(ty) == MTYPE_C10 ||
                                         TY_mtype(ty) == MTYPE_CQ);
} /* TY_Is_Complex */

inline BOOL TY_Is_Array_Or_Function(TY_IDX ty)
{
   return ty != 0 && (TY_Is_Function(ty) || TY_Is_Array(ty));
} /* TY_Is_Array_Or_Function */

inline BOOL TY_Is_Scalar(TY_IDX ty)
{
   return TY_kind(ty) == KIND_SCALAR;
} /* TY_Is_Scalar */

inline BOOL TY_Is_Pointer_Or_Scalar(TY_IDX ty)
{
   return TY_Is_Scalar(ty) || TY_Is_Pointer(ty);
} /* TY_Is_Pointer_Or_Scalar */

#if defined(BUILD_WHIRL2F)
inline BOOL TY_Is_Character_Reference(TY_IDX ty)
{
   return TY_Is_Pointer(ty) &&
          (TY_is_character(TY_pointed(ty)) ||
           TY_mtype(TY_pointed(ty)) == MTYPE_STR);
} /* TY_Is_Character_Reference */

/* The front-end is not always reliable in where it sets the is_character
 * flag, so we look for it both on the array and on the element type.
 */
inline BOOL TY_Is_Character_String(TY_IDX ty)
{
   return TY_is_character(ty) ||
          TY_mtype(ty) == MTYPE_STR ||
          (TY_Is_Array(ty)                 &&
           TY_Is_Integral(TY_AR_etype(ty)) &&
           TY_size(TY_AR_etype(ty)) == 1   &&
           TY_is_character(TY_AR_etype(ty)));
} /* TY_Is_Character_String */

inline BOOL TY_Is_Chararray(TY_IDX ty)
{
   return TY_Is_Array(ty) && TY_Is_Character_String(TY_AR_etype(ty));
} /* TY_Is_Chararray */

inline BOOL TY_Is_Chararray_Reference(TY_IDX ty)
{
   return TY_Is_Pointer(ty) && TY_Is_Chararray(TY_pointed(ty));
} /* TY_Is_Chararray_Reference */

#endif /*BUILD_WHIRL2F*/

inline BOOL TY_Is_Array_Of_Chars(TY_IDX ty)
{
   return TY_Is_Array(ty) &&
          TY_AR_ndims(ty) == 1 &&
          TY_Is_Integral(TY_AR_etype(ty)) &&
          TY_size(TY_AR_etype(ty)) == 1;
} /* TY_Is_Array_Of_Chars */


/* fortran FEs now generate U1 arrays for chars */

inline BOOL TY_Is_Array_Of_UChars(TY_IDX ty)
{
   return TY_Is_Array(ty) &&
          TY_AR_ndims(ty) == 1 &&
          TY_mtype(TY_AR_etype(ty)) == MTYPE_U1 ;
} /* TY_Is_Array_Of_Chars */

inline BOOL TY_Is_Preg_Type(TY_IDX ty)
{
   /* Return True if ty is a valid type for pseudo registers;
      return False otherwise. */
   return TY_Is_Pointer_Or_Scalar(ty);
} /* TY_Is_Preg_Type */

inline BOOL FLD_Is_Bitfield(FLD_HANDLE fld, FLD_HANDLE next_fld,
			    INT64 max_size) 
{
   /* fld must not be a member of a union! */
   return !FLD_equivalence(fld) &&
          (FLD_is_bit_field(fld) ||
           (next_fld.Is_Null () && max_size < TY_size(FLD_type(fld))) ||
           (!next_fld.Is_Null() && !FLD_equivalence(next_fld) &&
            FLD_ofst(next_fld) - FLD_ofst(fld) < TY_size(FLD_type(fld))));
} /* FLD_Is_Bitfield */

                  /*------ Symbol table Information ------*/
                  /*--------------------------------------*/

inline BOOL Stab_Is_Valid_Base(const ST *st)
{
   return (ST_base(st) != NULL &&
           ST_base(st) != (st) &&
           ST_sym_class(ST_base(st)) != CLASS_BLOCK /* cg generated */ );
} /* Stab_Is_Valid_Base */

inline BOOL Stab_Is_Common_Block(const ST *st)
{
   return ((ST_sclass(st) == SCLASS_COMMON ||
            ST_sclass(st) == SCLASS_DGLOBAL) &&
           TY_Is_Structured(ST_type(st)));
} /* Stab_Is_Common_Block */
      
inline BOOL Stab_Is_Equivalence_Block(const ST *st)
{
   return (ST_sym_class(st) == CLASS_VAR                 &&
           TY_Is_Structured(ST_type(st))                 &&
           ST_sclass(st) != SCLASS_COMMON                &&
           ! TY_flist(Ty_Table[ST_type(st)]).Is_Null ()  &&
           FLD_equivalence(TY_fld(Ty_Table[ST_type(st)])));
} /* Stab_Is_Equivalence_Block */

inline BOOL Stab_Is_Based_At_Common_Or_Equivalence(const ST *st)
{
   return (Stab_Is_Valid_Base(st) &&
           (Stab_Is_Common_Block(ST_base(st)) ||
            Stab_Is_Equivalence_Block(ST_base(st))));
} /* Stab_Is_Based_At_Common_Or_Equivalence */

inline BOOL Stab_No_Linkage(const ST *st)
{
   return (ST_sclass(st) == SCLASS_AUTO   ||
           ST_sclass(st) == SCLASS_FORMAL ||
           ST_sclass(st) == SCLASS_FORMAL_REF);
} /* Stab_No_Linkage */

inline BOOL Stab_External_Linkage(const ST *st)
{
   return (!Stab_No_Linkage(st)                   &&
           ST_sclass(st) != SCLASS_PSTATIC        &&
           ST_sclass(st) != SCLASS_FSTATIC        &&
           ST_sclass(st) != SCLASS_CPLINIT        &&
           ST_sclass(st) != SCLASS_EH_REGION      &&
           ST_sclass(st) != SCLASS_EH_REGION_SUPP &&
           ST_sclass(st) != SCLASS_DISTR_ARRAY);
} /* Stab_External_Linkage */

inline BOOL Stab_External_Def_Linkage(const ST *st)
{
   return (Stab_External_Linkage(st) && ST_sclass(st) != SCLASS_EXTERN);
} /* Stab_External_Def_Linkage */

inline BOOL Stab_Identical_Quals(TY_IDX t1, TY_IDX t2)
{
   return (TY_is_volatile(t1) == TY_is_volatile(t2) &&
           TY_is_restrict(t1) == TY_is_restrict(t2) &&
           TY_is_const(t1) == TY_is_const(t2));
} /* Stab_Identical_Quals */

inline BOOL Stab_Assign_Compatible_Pointer_Quals(TY_IDX t1, TY_IDX t2)
{
   return ((TY_is_volatile(t2)? TY_is_volatile(t1) : TRUE) &&
           (TY_is_restrict(t2)? TY_is_restrict(t1) : TRUE) &&
           (TY_is_const(t2)?    TY_is_const(t1) : TRUE));
} /* Stab_Assign_Compatible_Pointer_Quals */

#if defined(BUILD_WHIRL2F)
/* A macro to test if a parameter is a character string, in which case
 * it needs an implicit length parameter.  Note that in the test on the
 * argument (caller) side we only need to consider reference types, since
 * the call-by-reference always should be explicit on that side.  This
 * macro should only be used on the subprogram definition side!  This
 * only applies to Fortran code.
 */
inline BOOL STAB_PARAM_HAS_IMPLICIT_LENGTH(const ST *st)
{
   return ((ST_sclass(st) == SCLASS_FORMAL_REF &&
            TY_Is_Character_String(ST_type(st))) ||
           (ST_sclass(st) == SCLASS_FORMAL &&
            (TY_Is_Character_Reference(ST_type(st)) ||
             TY_Is_Chararray_Reference(ST_type(st)))));
}

/* Identify cases when a reference parameter is explicitly represented
 * as a pointer (as opposed to an SCLASS_FORMAL_REF).  This only applies
 * to Fortran code.
*/
inline BOOL STAB_IS_POINTER_REF_PARAM(const ST *st)
{
   return (TY_Is_Pointer(ST_type(st))   &&
           ST_sclass(st)==SCLASS_FORMAL &&
           !ST_is_value_parm(st));
}
#endif /*BUILD_WHIRL2F*/


                  /*----- Reserved Names Information -----*/
                  /*--------------------------------------*/

extern BOOL Stab_Reserved_Ty(TY_IDX ty);
extern BOOL Stab_Reserved_St(const ST *st);

                  /*------- Referenced Information -------*/
                  /*--------------------------------------*/

extern void Stab_Reset_Referenced_Flag(SYMTAB_IDX symtab);
   

               /*------ Function type attributes ------*/
               /*--------------------------------------*/

inline BOOL Func_Return_Character(TY_IDX func_ty)
{
#ifdef BUILD_WHIRL2F
   return TY_is_character(Ty_Table[TY_ret_type(func_ty)]);
#else /*BUILD_WHIRL2C*/
   return FALSE;
#endif /*BUILD_WHIRL2F*/
} /* Func_Return_Character */

inline TY_IDX Func_Return_Type(TY_IDX func_ty)
{
#ifdef BUILD_WHIRL2F
   return TY_ret_type(func_ty);
#else /*BUILD_WHIRL2C*/
   return TY_is_character(Ty_Table[TY_ret_type(func_ty)]) ?
                                             Void_Type : TY_ret_type(func_ty);
#endif /*BUILD_WHIRL2F*/
} /* Func_Return_Type */

inline BOOL Func_Return_To_Param(TY_IDX func_ty)
{
#ifdef BUILD_WHIRL2F
   return TY_return_to_param(Ty_Table[func_ty]);
#else /*BUILD_WHIRL2C*/
   return TY_return_to_param(Ty_Table[func_ty]) &&
          !TY_is_character(Ty_Table[TY_ret_type(func_ty)]);
#endif /*BUILD_WHIRL2F*/
} /* Func_Return_To_Param */



               /*------ Obtaining pointer/array types ------*/
               /*-------------------------------------------*/

inline TY_IDX
Stab_Pointer_To(TY_IDX pointee)          { return Make_Pointer_Type(pointee); }


extern TY_IDX Stab_Array_Of(TY_IDX etype, mINT64 num_elts) ;

extern void Stab_initialize(void);
extern void Stab_finalize(void);
extern void Stab_initialize_flags(void) ;
extern void Stab_finalize_flags(void) ;


                 /*------ Name manipulation ------*/
                 /*-------------------------------*/

extern void Stab_Free_Namebufs(void);
extern char *Get_Name_Buf_Slot(UINT size);
extern const char *Number_as_String(INT64 number, const char *fmt);
extern const char *Ptr_as_String(const void *ptr);
extern const char *Concat2_Strings(const char *name1, const char *name2);

inline const char *Concat3_Strings(const char *name1,
                                   const char *name2,
                                   const char *name3)
{
   return Concat2_Strings(name1, Concat2_Strings(name2, name3));
} /* Concat3_Strings */

extern UINT64 Get_Hash_Value_For_Name(const char *name);

inline UINT32 Name_Hash_Idx(UINT64 hash_value, INT32 tbl_size)
{
   return (UINT32)(hash_value % tbl_size);
} /* Name_Hash_Idx */

extern STAB_OFFSET Stab_Full_Split_Offset(const ST *split_out_st);


             /*---- temporary variable information -----*/
             /*-----------------------------------------*/

extern void Stab_Free_Tmpvars(void);
extern void Stab_Unlock_All_Tmpvars(void);
extern UINT Stab_Lock_Tmpvar(TY_IDX ty, void (*declare_tmpvar)(TY_IDX, UINT));
extern void Stab_Unlock_Tmpvar(UINT idx);

#endif /* stab_attr_INCLUDED */






