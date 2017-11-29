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


#ifndef PUinfo_INCLUDED
#define PUinfo_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: PUinfo.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:59-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.PUinfo.h $
 *
 * Revision history:
 *   26-Dec-94 - Original Version
 *
 * Description:
 *
 *   Maintains information about the scope and block associated with
 *   a program unit (function/procedure).  We have defined three
 *   data-structures for accessing information about call-sites,
 *   return-sites, and function return types respectively.  The
 *   interface is as follows:
 *
 *  Exported state variables:
 *  -------------------------
 *
 *    PUinfo_current_func:
 *      Denotes the OPR_FUNC_ENTRY node for the current function
 *      being processed.  This variable is initialized by 
 *      PUinfo_init(), and we provide macros to access the
 *      return type and the ST for the function.  
 *      PUINFO_RETURN_TO_PARAM is TRUE when the current PU returns
 *      through the first parameter (e.g. for large structs).
 *
 *    PUinfo_local_decls:
 *      This buffer is used to accumulate declarations for variables
 *      and registers belonging to the current PU.
 *
 *    PUinfo_returnPreg:
 *      Information about how the return type of "PUinfo_current_func"
 *      will be split up into return registers.  This variable is 
 *      set by a call to PUinfo_Get_ReturnPreg() through PUinfo_init().
 *
 *  Interface routines:
 *  -------------------
 *
 *    PUinfo_initialize:
 *      Given the current symbol table (presumably the global symbol
 *      table), this procedure will add its symbols to the name-table.
 *      Other initiation is on a per-PU basis.
 *
 *    PUinfo_finalize:
 *      Free up everything that was allocated to the heap through
 *      this package.
 *
 *    PUinfo_init_pu:
 *      Given the OPC_FUNC_ENTRY node for a PU, and a subtree under
 *      that block, this function will traverse the current symbol-
 *      table, and the subtree to accumulate information needed 
 *      in the actual translation of the given subtree to a HLL.  
 *      The other functions provided in the interface to this file 
 *      will allow us to access the information accumulated during 
 *      this prepass over the subtree.  Note that no WHIRL->HLL
 *      translation is expected outside the given subtree!
 *
 *    PUinfo_exit_pu:
 *      Free up the information accumulated by means of PUinfo_init_pu().
 *      This may not literally free up memory, but may make it
 *      available for reuse when processing the next PU.
 *
 *    PUinfo_Symbol_Owns_Name:
 *      Given the WHIRL2C_make_valid_c_name (for whirl2c) or the
 *      WHIRL2F_make_valid_name (for whirl2f) version of a name, 
 *      return TRUE if the "symbol" pointer "owns" the name; return
 *      FALSE if the name is empty (NULL or "\0") or when the given
 *      id pointer does not own the name.  Only one identifier in a 
 *      given scope can "own" each name, and only this identifier can
 *      therefore be declared with the unmangled version of the name.
 *      Use INVALID_PREG_NUM when the symbol does not denote a preg.
 *
 *    PUinfo_Symbol_Id:
 *      Return a unique identifying number for the given symbol.  Note
 *      that this applies to all symbol kinds, except SYMKIND_PREG,
 *      for which PUinfo_Symbol_Owns_Name()==FALSE.  If no identifying
 *      number has been recorded for the given symbol, the value
 *      returned is 0 (zero).
 *
 *    PUinfo_Get_CallSites:
 *    PUinfo_Get_ReturnSites:
 *      Get information about every call and return statement, in
 *      the order (inorder/lexicographical) in which these will be
 *      processed during the WHIRL to HLL translation.  Store statements
 *      that assign between return-registers and (temporary) variables
 *      or registers are identified to improve the translation to 
 *      high-level languages.  The idea is to minimize the use of
 *      return-registers.  When RETURNSITE_return_var() or
 *      CALLSITE_return_var() is NULL, nothing is known of (temporary) 
 *      variables/registers holding call or function result, and the
 *      return-registers must be used.
 *
 *   PUinfo_Preg_Type:
 *      The type associated with a preg reference in a WHIRL tree may
 *      not always correspond to what we declare in the high-level
 *      language, since some pregs may be referred to as more than
 *      one type (e.g. integral types of various sizes).  Always use
 *      this function to get the actual assumed type of a given preg.
 *
 *   PUinfo_Is_Preg_Declared:
 *      Return TRUE if the given preg-number has been marked as having
 *      been declared of the given type.  The type must be identical to
 *      what would be returned if it was passed to PUinfo_Preg_Type().
 *
 *   PUinfo_Set_Preg_Declared:
 *      Mark the given preg-number as having been declared of the
 *      given type.  The type must be identical to what would be
 *      returned if it was passed to PUinfo_Preg_Type().
 *
 *   PUinfo_Get_ReturnPreg:
 *      Returns information about how a return value of the given
 *      type will be split into return registers.  Note that before
 *      calling PUinfo_init(), we must use RETURN_PREG_mtype() to get
 *      the mtype of the registers, while after the analysis of
 *      PUinfo_init() we should always use RETURN_PREG_declared_mtype()!
 *      This is because we may have various uses of an integral preg as
 *      different integral types, but it will be declared as one
 *      integral type only and this type is determined based on the
 *      analysis of the preg uses in PUinfo_init().
 *
 *      Note that the return-type information reflects High-WHIRL,
 *      which has single pregs for complex and quad types.  In Mid-
 *      WHIRL, these are lowered to actual register sized pregs and 
 *      distributed over two return registers.
 *
 * ====================================================================
 * ====================================================================
 */

        /*----- Information about call sites -----*/
        /*----------------------------------------*/

typedef struct CallSite CALLSITE;
struct CallSite
{
   const WN   *call_wn;
   TY_IDX      return_ty;  /* Return type of the called function */
   const WN   *store1;     /* Store from return register1 (or NULL) */
   const WN   *store2;     /* Store from return register2 (or NULL) */
   const ST   *return_var; /* Returned value is stored into this ST ... */
   STAB_OFFSET var_offset; /* ... at this offset */
   BOOL        in_regs;    /* TRUE if value must be in return registers */
   CALLSITE   *next;       /* NULL if this is the last call-site in the PU */
};
#define CALLSITE_call(ci) (ci)->call_wn
#define CALLSITE_return_ty(ci) (ci)->return_ty
#define CALLSITE_store1(ci) (ci)->store1
#define CALLSITE_store2(ci) (ci)->store2
#define CALLSITE_return_var(ci) (ci)->return_var
#define CALLSITE_var_offset(ci) (ci)->var_offset
#define CALLSITE_in_regs(ci) (ci)->in_regs
#define CALLSITE_next(ci) (ci)->next


        /*----- Information about return sites -----*/
        /*------------------------------------------*/

typedef struct ReturnSite RETURNSITE;
struct ReturnSite
{
   const WN    *return_wn;
   const WN    *store1;     /* Store into return register1 (or NULL) */
   const WN    *store2;     /* Store into return register2 (or NULL) */
   const ST    *return_var; /* Return registers stored from this ST ... */
   STAB_OFFSET  var_offset; /* ... at this offset */
   RETURNSITE  *next;       /* NULL if this is the last return-site in the PU */
};
#define RETURNSITE_return(ci) (ci)->return_wn
#define RETURNSITE_store1(ci) (ci)->store1
#define RETURNSITE_store2(ci) (ci)->store2
#define RETURNSITE_return_var(ci) (ci)->return_var
#define RETURNSITE_var_offset(ci) (ci)->var_offset
#define RETURNSITE_next(ci) (ci)->next


  /*----- Information about pseudo registers for function returns -----*/
  /*-------------------------------------------------------------------*/

typedef struct RETURN_PREG
{
				       
   UINT8       num_pregs;  /* Number of pregs needed for return type */
   MTYPE       mtype[2];   /* Mtypes for the return pregs (or MTYPE_V) */
   STAB_OFFSET offset[2];  /* The preg-numbers for the return pregs */
} RETURN_PREG;
#define RETURN_PREG_num_pregs(rp) (rp)->num_pregs
#define RETURN_PREG_mtype(rp, i) (rp)->mtype[(i)]
#define RETURN_PREG_offset(rp, i) (rp)->offset[(i)]


  /*----- Information about the current function, return type etc. ----*/
  /*-------------------------------------------------------------------*/


extern const WN          *PUinfo_current_func;
extern const RETURN_PREG *PUinfo_return_preg;
extern TOKEN_BUFFER       PUinfo_local_decls;
extern TOKEN_BUFFER       PUinfo_pragmas;
extern UINT               PUinfo_local_decls_indent;

#define PUINFO_FUNC_ST_IDX WN_st_idx(PUinfo_current_func)
#define PUINFO_FUNC_ST     WN_st(PUinfo_current_func)
#define PUINFO_FUNC_TY     ST_pu_type(PUINFO_FUNC_ST)
#define PUINFO_FUNC_NAME   W2CF_Symtab_Nameof_St(PUINFO_FUNC_ST)

#define PUINFO_RETURN_TY        Func_Return_Type(PUINFO_FUNC_TY)
#define PUINFO_RETURN_PARAM     WN_st(WN_formal(PUinfo_current_func, 0))
#define PUINFO_RETURN_TO_PARAM  Func_Return_To_Param(PUINFO_FUNC_TY)
#define PUINFO_RETURN_CHARACTER Func_Return_Character(PUINFO_FUNC_TY)


        /*----- Interface routines -----*/
        /*------------------------------*/

extern void PUinfo_initialize(void);
extern void PUinfo_finalize(void);

extern void PUinfo_init_pu(const WN *wn, WN *body_part_of_interest);
extern void PUinfo_exit_pu(void);

extern CALLSITE *PUinfo_Get_CallSites(void);
extern RETURNSITE *PUinfo_Get_ReturnSites(void);

extern TY_IDX PUinfo_Preg_Type(TY_IDX preg_ty, INT16 preg_num);
extern BOOL PUinfo_Is_Preg_Declared(TY_IDX preg_ty, INT16 preg_num);
extern void PUinfo_Set_Preg_Declared(TY_IDX preg_ty, INT16 preg_num);

extern RETURN_PREG PUinfo_Get_ReturnPreg(TY_IDX return_ty);

#endif /* PUinfo_INCLUDED */

