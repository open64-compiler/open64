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


#include "wn.h"
#include "stab.h"
#include "strtab.h"
#include "mtypes.h"
#include "targ_const.h"
#include "config_targ.h"
#include "wn_util.h"
#include "region_util.h"
#include <alloca.h>
#include "data_layout.h"

extern "C" {
  void Rewrite_Pragmas_On_Structs (WN* block_wn, WN* wn);
}

static void Rewrite_Structs_In_MPRegion (WN* wn,
                                         WN* parent_wn,
                                         INT count,
                                         WN** rewrite_pwn,
                                         ST** rewrite_st,
                                         TYPE_ID* rewrite_rtype,
                                         TYPE_ID* rewrite_desc);
static BOOL Tree_Equiv (WN *wn1, WN* wn2);

/***********************************************************************
 *
 * Is the given wn an MP region?
 *
 ***********************************************************************/
static BOOL Is_Mp_Region(WN *wn)
{
  if (WN_opcode(wn) == OPC_REGION) {
    RID *rid = REGION_get_rid(wn);
    if (RID_TYPE_mp(rid)) return TRUE;
  }
  return FALSE;
}

/***********************************************************************
 *
 * Given an XPRAGMA reduction, return TRUE if a weird array element,
 * i.e. an element of an array with a weird base (array itself a struct
 * element, F90 allocatable array, etc)
 *
 ***********************************************************************/
static BOOL Weird_Array_Element (WN *pwn) {
  Is_True (pwn &&
           WN_operator(pwn) == OPR_XPRAGMA &&
           WN_pragma(pwn) == WN_PRAGMA_REDUCTION,
           ("Weird_Array_Element called weirdly"));

  if (WN_operator(WN_kid0(pwn)) != OPR_ARRAY) return FALSE;

  WN *array_base = WN_array_base(WN_kid0(pwn));
  OPERATOR opr = WN_operator(array_base);
  if ((opr == OPR_LDA &&
       TY_kind(ST_type(WN_st(array_base))) == KIND_ARRAY) ||
      (opr == OPR_LDID &&
       TY_kind(ST_type(WN_st(array_base))) == KIND_POINTER &&
       TY_kind(TY_pointed(ST_type(WN_st(array_base)))) == KIND_ARRAY))
    {
      // This looks well-behaved;
      return FALSE;
    }
  return TRUE;
}

/***********************************************************************
 *
 * Given an XPRAGMA reduction, return TRUE if the reduction is on an
 * array element, with the element itself being a struct, so that the
 * reduction itself is on a field within the struct. This field offset
 * is given by WN_pragma_arg2 field within the XPRAGMA node.
 *
 * E.g. 
 *  struct {
 *      int a;
 *      int b;
 *  } w[2], x, *y, *z[2];
 * and with reduction on w[i].a (case 1) or on (&x)[0].a (case 2) or on
 * y[0].a (case 3) or on (*z)[i].a (case 4).
 *
 * Cases 1 and 4 result from straightforward parallelization of user code.
 * Cases 2 and 3 can come up sometimes as a result of converting pointer
 * expressions to array references to allow analysis by LNO. 
 *
 ***********************************************************************/
static BOOL Array_Element_Then_Struct (WN *pwn) {
  
  Is_True (pwn &&
           WN_operator(pwn) == OPR_XPRAGMA &&
           WN_pragma(pwn) == WN_PRAGMA_REDUCTION,
           ("Array_Element_Then_Struct called weirdly"));

  if (WN_operator(WN_kid0(pwn)) != OPR_ARRAY) return FALSE;

  WN *array_base = WN_array_base(WN_kid0(pwn));
  OPERATOR opr = WN_operator(array_base);

  if (opr == OPR_LDA) {
    TY_IDX ty = ST_type(WN_st(array_base));

    if (TY_kind(ty) == KIND_ARRAY && TY_kind(TY_etype(ty)) == KIND_STRUCT)
      return TRUE;  // case 1

    if (TY_kind(ty) == KIND_STRUCT)
      return TRUE;  // case 2

  } else if (opr == OPR_LDID) {
    TY_IDX ty = ST_type(WN_st(array_base));

    if (TY_kind(ty) == KIND_POINTER) {
      if (TY_kind(TY_pointed(ty)) == KIND_STRUCT)
        return TRUE;  // case 3
      else if (TY_kind(TY_pointed(ty)) == KIND_ARRAY &&
               TY_kind(TY_etype(TY_pointed(ty))) == KIND_STRUCT)
        return TRUE;  // case 4
    }
  }

  return FALSE;
}

/***********************************************************************
 *
 * Given a WHIRL tree in wn, and the block node containing it in block_wn,
 * find all compiler-generated local/firstprivate/lastlocal/reduction pragmas
 * on structure elements, and replace them with simple scalars.
 *
 * Note that as a temporary hack, PFA-generated XPRAGMA reductions use
 * WN_prefetch_flag to encode the offset, since WN_pragma_arg2 is already
 * taken up by the reduction operator.
 *
 * (block_wn is required coz we don't have parent pointers).
 *
 ***********************************************************************/
extern void Rewrite_Pragmas_On_Structs (WN* block_wn, WN* wn) {
  if (!wn) return;

  if (Is_Mp_Region (wn)) {

    FmtAssert (block_wn, ("Rewrite_Pragmas: missing BLOCK node"));
    /*
     * first count the number of rewrite entries
     */
    WN* pwn = WN_first(WN_region_pragmas(wn));
    INT count = 0;
    while (pwn) {
      Is_True (WN_operator(pwn) == OPR_PRAGMA ||
               WN_operator(pwn) == OPR_XPRAGMA,
               ("Rewrite_Pragmas: Expected a pragma/xpragma node"));

      ST* st = NULL;
      if (WN_operator(pwn) == OPR_PRAGMA) st = WN_st(pwn);

      if (WN_operator(pwn) == OPR_PRAGMA &&
          WN_pragma_compiler_generated(pwn) &&
          (WN_pragma(pwn) == WN_PRAGMA_REDUCTION ||
           WN_pragma(pwn) == WN_PRAGMA_LOCAL ||
           WN_pragma(pwn) == WN_PRAGMA_FIRSTPRIVATE ||
           WN_pragma(pwn) == WN_PRAGMA_LASTLOCAL) &&
          (TY_kind(ST_type(st)) == KIND_STRUCT)) {

        // compiler generated local/firstprivate/lastlocal/reduction
        // on a struct element
        count++;
      }
      else if (WN_operator(pwn) == OPR_XPRAGMA &&
               WN_pragma_compiler_generated(pwn) &&
               WN_pragma(pwn) == WN_PRAGMA_REDUCTION &&
               WN_operator(WN_kid0(pwn)) == OPR_ARRAY) {

        // this is an array-element reduction. 
        // let regular Fortran array-element reductions go through,
        // but rewrite all the weird cases.
        
        WN* array_base = WN_array_base(WN_kid0(pwn));
        OPERATOR opr = WN_operator(array_base);
        
        if (Array_Element_Then_Struct(pwn)) {
          // compiler generated reduction on an array element, where
          // the element type of the array is a struct.
          count++;

        } else if (Weird_Array_Element(pwn)) {
          // compiler generated reduction on an array element,
          // but with a weird base (struct element, F90 allocatable array)
          count++;
        }

      }
      pwn = WN_next(pwn);
    }

    if (count) {
      /* ok, now we know how many symbols to rewrite. store them */
      WN** rewrite_pwn = (WN**) alloca (count*sizeof(WN*));
      ST** rewrite_st  = (ST**) alloca (count*sizeof(ST*));
      TYPE_ID* rewrite_rtype = (TYPE_ID*) alloca (count*sizeof(TYPE_ID));
      TYPE_ID* rewrite_desc = (TYPE_ID*) alloca (count*sizeof(TYPE_ID));
      INT i = 0;
      pwn = WN_first(WN_region_pragmas(wn));
      while (pwn) {
        ST* st = NULL;
        if (WN_operator(pwn) == OPR_PRAGMA) st = WN_st(pwn);

        if (WN_operator(pwn) == OPR_PRAGMA &&
            WN_pragma_compiler_generated(pwn) &&
            (WN_pragma(pwn) == WN_PRAGMA_REDUCTION ||
             WN_pragma(pwn) == WN_PRAGMA_LOCAL ||
             WN_pragma(pwn) == WN_PRAGMA_FIRSTPRIVATE ||
             WN_pragma(pwn) == WN_PRAGMA_LASTLOCAL) &&
            (TY_kind(ST_type(st)) == KIND_STRUCT)) {

          /*
           * compiler generated local/firstprivate/lastlocal/reduction
           * on a struct element.
           * So do the rewrite.
           */

          FmtAssert (i<count, ("Rewrite_STs. counting error"));
          // check for duplicates
          BOOL duplicate = FALSE;
          for (INT j=0; j<i; j++) {
            if (WN_st(rewrite_pwn[j]) == WN_st(pwn) &&
                WN_pragma_arg1(rewrite_pwn[j]) == WN_pragma_arg1(pwn)) {
              if (WN_pragma(rewrite_pwn[j]) == WN_pragma(pwn)) {
                duplicate = TRUE;
                // delete the redundant pragma
                WN* tmp_wn = pwn;
                pwn = WN_prev(pwn);
                WN_DELETE_FromBlock (WN_region_pragmas(wn), tmp_wn);
                break;
              }
              else {
                FmtAssert (FALSE, ("Rewrite_Pragmas: contradictory pragmas"));
              }
            }
          }
          if (!duplicate) {
            rewrite_pwn[i] = pwn;
            rewrite_st[i] = NULL;
            i++;
          }
        }
        else if (WN_operator(pwn) == OPR_XPRAGMA &&
                 WN_pragma_compiler_generated(pwn) &&
                 WN_pragma(pwn) == WN_PRAGMA_REDUCTION &&
                 WN_operator(WN_kid0(pwn)) == OPR_ARRAY) {

          WN* array_base = WN_array_base(WN_kid0(pwn));
          OPERATOR opr = WN_operator(array_base);

          if (Array_Element_Then_Struct(pwn) ||
              Weird_Array_Element(pwn)) {

            // check for duplicates
            BOOL duplicate = FALSE;
            for (INT j=0; j<i; j++) {
              if (Tree_Equiv(rewrite_pwn[j], pwn)) {
                duplicate = TRUE;
                // delete the redundant pragma
                WN* tmp_wn = pwn;
                pwn = WN_prev(pwn);
                WN_DELETE_FromBlock (WN_region_pragmas(wn), tmp_wn);
                break;
              }
            }
            if (!duplicate) {
              rewrite_pwn[i] = pwn;
              rewrite_st[i] = NULL;
              i++;
            }
          }
        }
        pwn = WN_next(pwn);
      }

      count = i;
      /* now rewrite all pragmas that need rewriting */
      Rewrite_Structs_In_MPRegion (WN_region_body(wn),
                                   wn,
                                   count,
                                   rewrite_pwn,
                                   rewrite_st,
                                   rewrite_rtype,
                                   rewrite_desc);

      /* now do the appropriate initialization etc */
      for (i=0; i<count; i++) {

        // if we didn't even find a reference to the variable in the 
        // MP-region body, then we don't have to do anything.
        if (rewrite_st[i] == NULL) continue;

        pwn = rewrite_pwn[i];

        if (WN_operator(pwn) == OPR_XPRAGMA) {
          // array reduction

          /* initialization of new-symbol */
          OPCODE opc = OPCODE_make_op(OPR_ILOAD,
                                      rewrite_rtype[i],
                                      rewrite_desc[i]);
          WN* iload_wn = WN_CreateIload (opc, WN_prefetch_flag(pwn),
                                         ST_type(rewrite_st[i]),
                                         Make_Pointer_Type(ST_type
                                                           (rewrite_st[i]),
                                                           FALSE),
                                         WN_COPY_Tree(WN_kid0(pwn)));
          
          opc = OPCODE_make_op (OPR_STID, MTYPE_V, rewrite_desc[i]);
          WN* stid_wn = WN_CreateStid (opc, 0, rewrite_st[i],
                                       ST_type(rewrite_st[i]),
                                       iload_wn);
          WN_INSERT_BlockBefore (block_wn, wn, stid_wn);

          /* finalization of new symbol */
          opc = OPCODE_make_op(OPR_LDID,
                               rewrite_rtype[i],
                               rewrite_desc[i]);
          WN *ldid_wn = WN_CreateLdid (opc,
                                       0,
                                       rewrite_st[i],
                                       ST_type(rewrite_st[i]));
          opc = OPCODE_make_op (OPR_ISTORE, MTYPE_V, rewrite_desc[i]);
          WN* istore_wn = WN_CreateIstore (opc, WN_prefetch_flag(pwn),
                                           Make_Pointer_Type(ST_type
                                                             (rewrite_st[i]),
                                                             FALSE),
                                           ldid_wn,
                                           WN_COPY_Tree(WN_kid0(pwn)));
          WN_INSERT_BlockAfter (block_wn, wn, istore_wn);

          // create a new pragma, delete the old one
          WN* pragma_wn = WN_CreatePragma (WN_PRAGMA_REDUCTION,
                                           rewrite_st[i], 0, 0);
            // PV 525199: need to copy over reduction operator
          WN_pragma_arg2(pragma_wn) = WN_pragma_arg2(pwn);
          WN_INSERT_BlockBefore (WN_region_pragmas(wn), pwn, pragma_wn);
          WN_DELETE_FromBlock (WN_region_pragmas(wn), pwn);
          continue;
        }
        else {
          ST* st = WN_st(pwn);

          switch (WN_pragma(pwn)) {
          case WN_PRAGMA_REDUCTION:
            {
              /* initialization of new-symbol */
              OPCODE opc = OPCODE_make_op(OPR_LDID,
                                          rewrite_rtype[i],
                                          rewrite_desc[i]);
              WN* ldid_wn = WN_CreateLdid (opc,
                                           WN_pragma_arg1(pwn),
                                           st,
                                           ST_type(rewrite_st[i]));
              opc = OPCODE_make_op (OPR_STID, MTYPE_V, rewrite_desc[i]);
              WN* stid_wn = WN_CreateStid (opc, 0, rewrite_st[i],
                                           ST_type(rewrite_st[i]),
                                           ldid_wn);
              WN_INSERT_BlockBefore (block_wn, wn, stid_wn);

              /* finalization of new symbol */
              opc = OPCODE_make_op(OPR_LDID,
                                   rewrite_rtype[i],
                                   rewrite_desc[i]);
              ldid_wn = WN_CreateLdid (opc,
                                       0,
                                       rewrite_st[i],
                                       ST_type(rewrite_st[i]));
              opc = OPCODE_make_op (OPR_STID, MTYPE_V, rewrite_desc[i]);
              stid_wn = WN_CreateStid (opc, WN_pragma_arg1(pwn),
                                       st,
                                       ST_type(rewrite_st[i]),
                                       ldid_wn);
              WN_INSERT_BlockAfter (block_wn, wn, stid_wn);

              /* now we can rewrite the pragma */
              WN_st_idx(pwn) = ST_st_idx(rewrite_st[i]);
              WN_pragma_arg1(pwn) = 0;
              break;
            }
          case WN_PRAGMA_LOCAL:
            {
              /* just rewrite the pragma */
              WN_st_idx(pwn) = ST_st_idx(rewrite_st[i]);
              WN_pragma_arg1(pwn) = 0;
              break;
            }
          case WN_PRAGMA_FIRSTPRIVATE:
            {
              /* initialization of new symbol */

              // it is remotely possible that we saw only an STID, 
              // but didn't see an LDID of the symbol at all.
              // And the symbol is live-out.
              // In which case rewrite_rtype will be void. 
              // Just use rewrite_desc in that case.
              OPCODE opc = OPCODE_make_op(OPR_LDID,
                                          (rewrite_rtype[i] != MTYPE_V ?
                                           rewrite_rtype[i] : rewrite_desc[i]),
                                          rewrite_desc[i]);
              WN *ldid_wn = WN_CreateLdid (opc,
                                           WN_pragma_arg1(pwn),
                                           WN_st(pwn),
                                           ST_type(rewrite_st[i]));
              opc = OPCODE_make_op (OPR_STID, MTYPE_V, rewrite_desc[i]);
              WN* stid_wn = WN_CreateStid (opc,
                                           0,
                                           rewrite_st[i],
                                           ST_type(rewrite_st[i]),
                                           ldid_wn);

              WN_INSERT_BlockFirst (WN_region_body(wn), stid_wn);

              /* now we can rewrite the pragma */
              WN_st_idx(pwn) = ST_st_idx (rewrite_st[i]);
              WN_pragma_arg1(pwn) = 0;
              break;
            }
          case WN_PRAGMA_LASTLOCAL:
            {
              /* finalization of new symbol */

              // it is remotely possible that we saw only an STID, 
              // but didn't see an LDID of the symbol at all.
              // And the symbol is live-out.
              // In which case rewrite_rtype will be void. 
              // Just use rewrite_desc in that case.
              OPCODE opc = OPCODE_make_op(OPR_LDID,
                                          (rewrite_rtype[i] != MTYPE_V ?
                                           rewrite_rtype[i] : rewrite_desc[i]),
                                          rewrite_desc[i]);
              WN* ldid_wn = WN_CreateLdid (opc,
                                           0,
                                           rewrite_st[i],
                                           ST_type(rewrite_st[i]));
              opc = OPCODE_make_op (OPR_STID, MTYPE_V, rewrite_desc[i]);
              WN* stid_wn = WN_CreateStid (opc, WN_pragma_arg1(pwn),
                                           st,
                                           ST_type(rewrite_st[i]),
                                           ldid_wn);
              WN_INSERT_BlockAfter (block_wn, wn, stid_wn);

              /* now we can rewrite the pragma */
              WN_st_idx(pwn) = ST_st_idx(rewrite_st[i]);
              WN_pragma_arg1(pwn) = 0;
              break;
            }
          default:
            {
              FmtAssert (FALSE, ("Rewrite_Pragmas: Unknown pragma type"));
            }
          }
        }
      }
    }
  }

  if (WN_opcode(wn) == OPC_BLOCK) {
    WN* kid = WN_first(wn);
    while (kid) {
      Rewrite_Pragmas_On_Structs (wn, kid);
      kid = WN_next(kid);
    }
  }
  else {
    for (INT i=0; i<WN_kid_count(wn); i++) {
      Rewrite_Pragmas_On_Structs (NULL, WN_kid(wn,i));
    }
  }
}

/***********************************************************************
 *
 * See if any of the pragmas in the rewrite_pwn pragma list matches
 * the given ST and ofst. If so, return its index, otherwise return -1.
 *
 ***********************************************************************/
static INT Find_Symbol (WN** rewrite_pwn, INT count, ST* st, WN_OFFSET ofst) {

  for (INT i=0; i<count; i++) {
    ST* this_st = NULL;
    if (WN_operator(rewrite_pwn[i]) == OPR_PRAGMA) {
      this_st = WN_st(rewrite_pwn[i]);
    }

    if (this_st == st && WN_pragma_arg1(rewrite_pwn[i]) == ofst)
      return i;
  }
  return -1;
}

/***********************************************************************
 *
 * Return true if the sub-trees wn1 and wn2 and "equiv"
 * (the equiv notion is similar to a recursive WN_Equiv).
 *
 ***********************************************************************/
static BOOL Tree_Equiv (WN *wn1, WN* wn2) {

  if (!wn1 && !wn2) return TRUE;    // both are NULL
  if (!wn1 || !wn2) return FALSE;   // one (but not both) is NULL
  if (!WN_Equiv (wn1, wn2)) return FALSE;   // not the same

  // now examine the kids
  if (WN_opcode(wn1) == OPC_BLOCK) {
    WN *kid1 = WN_first(wn1);
    WN *kid2 = WN_first(wn2);
    while (1) {
      if (!Tree_Equiv (kid1, kid2)) return FALSE;
      if (kid1 == NULL) break;
      kid1 = WN_next (kid1);
      kid2 = WN_next (kid2);
    };
    return TRUE;
  }
  else {
    // since the two nodes are equiv, they have the same # of children
    for (INT i=0; i<WN_kid_count(wn1); i++)
      if (!Tree_Equiv (WN_kid(wn1,i), WN_kid(wn2,i))) return FALSE;
    return TRUE;
  }
}

/***********************************************************************
 *
 * See if any of the Xpragmas in the rewrite_pwn pragma list match
 * the given array_wn. If so, return its index, otherwise return -1.
 *
 ***********************************************************************/
static INT Find_Reduction_Symbol (WN** rewrite_pwn,
                                  INT count,
                                  WN* array_wn,
                                  INT64 ofst) {

  for (INT i=0; i<count; i++) {
    WN* pwn = rewrite_pwn[i];
    if ((WN_operator(pwn) == OPR_XPRAGMA) &&
        Tree_Equiv (WN_kid0(pwn), array_wn) &&
        ofst == WN_prefetch_flag(pwn)) {
      return i;
    }
  }
  return -1;
}

/***********************************************************************
 *
 * Given a WHIRL tree in "wn", find all LDID/STID that refer to
 * a struct-symbol in rewrite_pwn. For each such ldid/stid, replace
 * it with a reference to the new st from rewrite_st. 
 *
 * rewrite_rtype and rewrite_desc store the rtype and desc of the load/store.
 *
 ***********************************************************************/
static void Rewrite_Structs_In_MPRegion (WN* wn,
                                         WN* parent_wn,
                                         INT count,
                                         WN** rewrite_pwn,
                                         ST** rewrite_st,
                                         TYPE_ID* rewrite_rtype,
                                         TYPE_ID* rewrite_desc) {

  if (!wn) return;
  
  /* rewrite memory ops - only LDID/STIDs, since scalar struct
   * elements should never be referenced any other way.
   * Also, we don't need to examine pragmas, since lastlocal and reduction
   * can only appear on the PDO, not a PARALLEL region, and if there is a
   * LOCAL on the parallel region, then we won't have any pragma on the
   * nested PDO. (And even if we do, we can safely localize the struct element
   * for the entire parallel region and be correct.
   */

  OPERATOR opr = WN_operator(wn);

  if (opr == OPR_LDID || opr == OPR_STID) {
    ST* st = WN_st(wn);

    INT idx = Find_Symbol (rewrite_pwn, count, st, WN_offset(wn));
    if (idx != -1) {
      if (rewrite_st[idx] == NULL) {
        /* create the replacement ST. Always create it on the stack */

        char* name = (char*) alloca(strlen(ST_name(st))+10);
        sprintf (name, "rewrite_%s", ST_name(st));

        ST* new_st = New_ST(CURRENT_SYMTAB);
        ST_Init (new_st,
                 Save_Str(name),
                 ST_class(st),
                 SCLASS_AUTO,
                 EXPORT_LOCAL,
                 WN_ty(wn));

        rewrite_st[idx] = new_st;
        rewrite_desc[idx] = WN_desc(wn);
        rewrite_rtype[idx] = WN_rtype(wn);
      }
      // rtype may still be garbage if we saw a store first
      if (opr == OPR_LDID && rewrite_rtype[idx] == MTYPE_V) {
        rewrite_rtype[idx] = WN_rtype(wn);
        // update desc as well, to be consistent
        rewrite_desc[idx] = WN_desc(wn);
      }
      
      WN_st_idx(wn) = ST_st_idx(rewrite_st[idx]);
      WN_offset(wn) = 0;
    }
  }

  // now check for an array element reduction.
  // Handle ILOAD and ISTORE of the array, as well as just the ARRAY node
  // in the case when it's a parameter. (This case will arise when PFA
  // becomes able to parallelize inter-procedural reductions.)

  if (opr == OPR_ILOAD || opr == OPR_ISTORE ||
      (opr == OPR_ARRAY &&
       WN_operator(parent_wn) != OPR_ILOAD &&
       WN_operator(parent_wn) != OPR_ISTORE)) {

    WN* array_wn = (opr == OPR_ARRAY ? wn :
                    (opr == OPR_ILOAD ? WN_kid0(wn) :
                     WN_kid1(wn)));

    if (WN_operator(array_wn) == OPR_ARRAY) {

      INT idx = Find_Reduction_Symbol (rewrite_pwn,
                                       count,
                                       array_wn,
                                       ((opr == OPR_ILOAD || OPR_ISTORE) ?
                                        WN_offset(wn) : 0));
      if (idx != -1) {

#ifdef Is_True_On
        if (opr == OPR_ARRAY) {
            // any use of reduction ARRAY node other than load/store/
	    // pass as param. probably signals a bug in PFA
          Is_True(WN_operator(parent_wn) == OPR_PARM, 
                  ("Rewrite_Structs_In_MPRegion(): bad parent opr == %d",
	           (INT) WN_operator(parent_wn)));
        }
#endif

        if (rewrite_st[idx] == NULL) {
          /* create the replacement ST. Always create it on the stack */

          char* name = (char*) alloca(20);
          sprintf (name, "rewrite_xreducn");

          ST* new_st = New_ST(CURRENT_SYMTAB);

          TY_IDX new_ty_idx;
          if (opr == OPR_ISTORE)
	    new_ty_idx = TY_pointed(WN_ty(wn));
	  else if (opr == OPR_ARRAY)
	    new_ty_idx = WN_ty(parent_wn);
	  else
	    new_ty_idx = WN_ty(wn);

          ST_Init (new_st,
                   Save_Str(name),
                   CLASS_VAR,
                   SCLASS_AUTO,
                   EXPORT_LOCAL,
                   new_ty_idx);

          rewrite_st[idx] = new_st;
          rewrite_desc[idx] = WN_desc(wn);
          rewrite_rtype[idx] = WN_rtype(wn);
        }
        if (opr == OPR_ILOAD && rewrite_rtype[idx] == MTYPE_V) {
          rewrite_rtype[idx] = WN_rtype(wn);
        }
        
        // replace the iload/istore with ldid/stid of the new scalar
        switch (opr) {

        case OPR_ILOAD: {
          OPCODE opc = OPCODE_make_op(OPR_LDID,
                                      rewrite_rtype[idx],
                                      rewrite_desc[idx]);
          WN *ldid_wn = WN_CreateLdid (opc, 0, rewrite_st[idx],
                                       ST_type(rewrite_st[idx]));
          FmtAssert (WN_opcode(parent_wn) != OPC_BLOCK,
                     ("Rewrite_pragmas: iload under a BLOCK node"));
	  INT kidno;
          for (kidno=0; kidno<WN_kid_count(parent_wn); kidno++) {
            if (WN_kid(parent_wn,kidno) == wn) break;
          }
          FmtAssert (kidno < WN_kid_count(parent_wn),
                     ("Rewrite_Pragmas: Could not find kid in parent"));
          WN_DELETE_Tree (WN_kid(parent_wn, kidno));
          WN_kid(parent_wn, kidno) = ldid_wn;
          wn = ldid_wn;
          break;
        }

        case OPR_ISTORE: {
          OPCODE opc = OPCODE_make_op (OPR_STID, MTYPE_V, rewrite_desc[idx]);
          WN* stid_wn = WN_CreateStid (opc, 0, rewrite_st[idx],
                                       ST_type(rewrite_st[idx]),
                                       WN_COPY_Tree(WN_kid0(wn)));
          FmtAssert (WN_opcode(parent_wn) == OPC_BLOCK,
                     ("Rewrite_pragmas: istore not under a BLOCK node"));
          WN_INSERT_BlockBefore (parent_wn, wn, stid_wn);
          WN_DELETE_FromBlock (parent_wn, wn);
          wn = stid_wn;
          break;
        }

        case OPR_ARRAY: {
          OPCODE opc = OPCODE_make_op (OPR_LDA, Pointer_type, MTYPE_V);
          WN* lda_wn =
            WN_CreateLda (opc, 0,
                          Make_Pointer_Type(ST_type(rewrite_st[idx]), FALSE),
                          rewrite_st[idx]);
          FmtAssert (WN_opcode(parent_wn) != OPC_BLOCK,
                     ("Rewrite_pragmas: array under a BLOCK node"));
	  INT kidno;
          for (kidno=0; kidno<WN_kid_count(parent_wn); kidno++) {
            if (WN_kid(parent_wn,kidno) == wn) break;
          }
          FmtAssert (kidno < WN_kid_count(parent_wn),
                     ("Rewrite_Pragmas: Could not find kid in parent"));
          WN_DELETE_Tree (WN_kid(parent_wn, kidno));
          WN_kid(parent_wn, kidno) = lda_wn;
          wn = lda_wn;
          break;
        }
        }
      }
    }
  }

  if (WN_opcode(wn) == OPC_BLOCK) {
    WN* kid = WN_first(wn);
    while (kid) {
      Rewrite_Structs_In_MPRegion (kid,
                                   wn,
                                   count,
                                   rewrite_pwn,
                                   rewrite_st,
                                   rewrite_rtype,
                                   rewrite_desc);
      kid = WN_next(kid);
    }
  }
  else {
    for (INT i=0; i<WN_kid_count(wn); i++) {
      Rewrite_Structs_In_MPRegion (WN_kid(wn,i),
                                   wn,
                                   count,
                                   rewrite_pwn,
                                   rewrite_st,
                                   rewrite_rtype,
                                   rewrite_desc);
    }
  }
}
