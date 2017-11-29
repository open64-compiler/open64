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



/***********************************************************************
 *
 * Exported Type
 *
 *  class SINGLE_ARRAY_REF_PREF
 *
 *      This class stores the following
 *          - base symbol of an array
 *          - prefetch whirl nodes generated through manual prefetch pragmas
 *          - whirl nodes that access the same array
 *      all for references/prefetches within the same do-loop,
 *      (and at the same nesting level). This data structures allows us 
 *       to easily match the prefetches to the corresponding references.
 *
 * Exported Functions
 *  
 *  SINGLE_ARRAY_REF_PREF (SYMBOL* s)
 *      Constructor that takes a symbol and creates an instance.
 *
 *  BOOL Same_Base_Symbol (SYMBOL* sym)
 *      Given sym, returns TRUE if sym and the symbol of this instance
 *      are the same, FALSE otherwise.
 *
 *  void Insert_Pref (WN* pragma, WN* pf)
 *      Given a manual prefetch node in pf and the corresponding
 *      pragma node in pragma_wn, store the prefetch and the pragma node.
 *
 *  void Insert_Ref (WN* ref)
 *      Store the supplied ref.
 *
 *  void Match_Refs_Prefs ()
 *      For each manual prefetch, try to find a matching reference
 *      and attach the two.
 *
 *  void Print (FILE* fp)
 *      Print the object.
 *
 *
 * Exported Type
 *
 *  class SINGLE_LOOP
 *      This class basically contains an array of the above data structure, 
 *      one for each base array.
 *
 * Exported Functions
 *
 *  SINGLE_LOOP (MEM_POOL* pool)
 *      Constructor
 *
 *  Process_Loop_Manual (WN* wn)
 *      Given a tree, this function first matches the 
 *      manual prefetches to the references within each loop.
 *
 *  Print (FILE* fp)
 *      Print routine.
 *
 *
 * Exported Type
 *
 *  class MANUAL_PREFETCH_SYMBOLS
 *      This class stores a list of all the manually prefetched symbols
 *      so that they can be ignored by Auto-Prefetching. It also stores
 *      the user-specified volume for each symbol.
 *
 * Exported Functions
 *
 *  MANUAL_PREFETCH_SYMBOLS (MEM_POOL* pool)
 *      Constructor
 *
 *  ~MANUAL_PREFETCH_SYMBOLS ()
 *      Destructor
 *
 *  void Enter (SYMBOL* sym, INT64 vol)
 *      Add the given symbol and vol to the list.
 *
 *  BOOL In_Manual (SYMBOL* sym)
 *      Return TRUE if given sym has been manually prefetched,
 *      FALSE otherwise.
 *
 *  INT64 Volume (SYMBOL* sym)
 *      Returns the (user)-specified volume for the given symbol.
 *
 *  void Print (FILE* fp)
 *      Print routine.
 *
 *
 ***********************************************************************/

#ifndef pf_manual_INCLUDED
#define pf_manual_INCLUDED

#include "prefetch.h"
#include "lnoutils.h"
#include "pf_common.h"

extern void dump_tree (WN *wn);
extern void fdump_tree (FILE*, WN*);
extern void dump_wn (WN *wn);
extern void fdump_wn (FILE*, WN *wn);

class SINGLE_ARRAY_REF_PREF {
  SYMBOL            _symb;      /* symbols aren't super accurate
                                 * in the presence of equivalenced
                                 * arrays, but let it go for now -- minor
                                 * optimization.
                                 */
  DYN_ARRAY<WN*>    _pref_da;   /* list of prefetch nodes */
  DYN_ARRAY<WN*>    _pragma_da; /* matching pragma nodes */
  DYN_ARRAY<WN*>    _ref_da;
  SINGLE_ARRAY_REF_PREF (void);
  SINGLE_ARRAY_REF_PREF (const SINGLE_ARRAY_REF_PREF&);
  SINGLE_ARRAY_REF_PREF* operator= (const SINGLE_ARRAY_REF_PREF&);
public:
  SINGLE_ARRAY_REF_PREF (SYMBOL* s) : _symb(s) {
    _pref_da.Set_Mem_Pool (PF_mpool);
    _pragma_da.Set_Mem_Pool (PF_mpool);
    _ref_da.Set_Mem_Pool (PF_mpool);
  }
  BOOL Same_Base_Symbol (SYMBOL* sym) {
    if ((_symb == *sym) && (_symb.St() == sym->St()))
      /* Checking that the actual STs are exactly equal is 
       * extra conservative in the presence of equivalenced arrays.
       * But the alternative is a little messy at this point since
       * DEPV_COMPUTE::Base_Test doesn't accept prefetches.
       * So we end up being conservative, and at least avoid the problems
       * of equivalenced arrays with different number of dimensions.
       * Also, with manual prefetches, the user is likely to insert a pragma
       * for the referenced array, not an equivalenced alias.
       */
      return TRUE;
    else return FALSE;
  }
  void Insert_Pref (WN* pragma, WN* pf) {
    _pref_da[_pref_da.Newidx()] = pf;
    _pragma_da[_pragma_da.Newidx()] = pragma;
  }
  void Insert_Ref (WN* ref) {
    _ref_da[_ref_da.Newidx()] = ref;
  }
  void Match_Refs_Prefs ();
  void Print (FILE* fp) {
    fprintf (fp, "Symbol: "); _symb.Print (fp); fprintf (fp, "\n");
    fprintf (fp, "References:\n");
    INT i;
    for (i=0; i<_ref_da.Elements(); i++)
      fdump_tree (fp, _ref_da[i]);
    fprintf (fp, "Prefetches:\n");
    for (i=0; i<_pref_da.Elements(); i++) {
      fdump_tree (fp, _pragma_da[i]);
      fdump_tree (fp, _pref_da[i]);
    }
    fflush (fp);
  }
};

class SINGLE_LOOP {
  DYN_ARRAY<SINGLE_ARRAY_REF_PREF*> _sarp_list;
  SINGLE_LOOP (void);
  SINGLE_LOOP (const SINGLE_LOOP&);
  SINGLE_LOOP* operator= (const SINGLE_LOOP&);
  void Insert_Pref (WN* pragma_wn, WN* pf_wn);
  void Insert_Ref  (WN* array_wn);
  void Collect_Refs_Prefs (WN* wn);
  void Match_Refs_Prefs (WN*) {
    // DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(PF_mpool), PF_mpool);
    // Build_Doloop_Stack(wn, stack);
    for (INT i=0; i<_sarp_list.Elements(); i++)
      _sarp_list[i]->Match_Refs_Prefs ();
    // CXX_DELETE (stack, PF_mpool);
  }
public:
  SINGLE_LOOP (MEM_POOL* pool) : _sarp_list(pool) {
    // allocate the index for bucket 0 always
    SYMBOL s;
    _sarp_list[_sarp_list.Newidx()] = CXX_NEW (SINGLE_ARRAY_REF_PREF(&s),
                                               PF_mpool);
    FmtAssert (_sarp_list.Lastidx() == 0,
               ("Improperly initialized symb_list dynamic array"));
  }
  void Process_Loop_Manual (WN* wn) {
    Collect_Refs_Prefs (wn);
    VB_PRINT (Print(stdout));
    Match_Refs_Prefs (wn);
  }
  void Print (FILE* fp) {
    fprintf (fp, "Collected refs prefs. Symbols are:\n");
    for (INT i=1; i<_sarp_list.Elements(); i++)
      _sarp_list[i]->Print(fp);
  }
};

class MANUAL_PREFETCH_SYMBOLS {
  DYN_ARRAY<SYMBOL*>    _sym_da;    // the symbol
  DYN_ARRAY<INT64>      _vol_da;    // it's volume in bytes
  MANUAL_PREFETCH_SYMBOLS (void);
  MANUAL_PREFETCH_SYMBOLS (const MANUAL_PREFETCH_SYMBOLS&);
  MANUAL_PREFETCH_SYMBOLS* operator= (const MANUAL_PREFETCH_SYMBOLS&);
public:
  MANUAL_PREFETCH_SYMBOLS (MEM_POOL* pool) : _sym_da(pool), _vol_da(pool) {
    FmtAssert ((_sym_da.Elements() == 0) && (_vol_da.Elements() == 0),
               ("#sym and #vol not zero"));
  }
  ~MANUAL_PREFETCH_SYMBOLS () {
    FmtAssert (_sym_da.Elements() == _vol_da.Elements(),
               ("Mismatch in #sym and #vol"));
    for (INT i=0; i<_sym_da.Elements(); i++) {
      CXX_DELETE (_sym_da[i], PF_mpool);
    }
  }
  /* Always create a new symbol, leave the incoming symbol untouched.
   */
  void Enter (SYMBOL* sym, INT64 vol);
  BOOL In_Manual (SYMBOL* sym) {
    for (INT i=0; i<_sym_da.Elements(); i++) {
      if (*sym == *(_sym_da[i])) return TRUE;
    }
    return FALSE;
  }
  INT64 Volume (SYMBOL* sym) {
    for (INT i=0; i<_sym_da.Elements(); i++) {
      if (*sym == *(_sym_da[i])) return _vol_da[i];
    }
    FmtAssert (FALSE, ("Could not find the given symbol"));
    return 0;
  }
  void Print (FILE* fp) {
    fprintf (fp, "Manually prefetched symbols are:-- \n");
    for (INT i=0; i<_sym_da.Elements(); i++) {
      fprintf (fp, "    "); _sym_da[i]->Print (fp);
      fprintf (fp, ", vol = %lld\n", _vol_da[i]);
    }
  }
};

extern MANUAL_PREFETCH_SYMBOLS* mpf_syms;

#endif // pf_manual_INCLUDED
