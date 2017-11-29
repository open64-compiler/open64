/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


#ifndef sxlist_INCLUDED
#define sxlist_INCLUDED "sxlist.h"

// class SX_PNODE : public CHAIN_NODE
//
// This data structure describes how a scalar is used within a loop.
// Actually, it precomputes information about privatizability assuming
// transformations may only occur beween loops s1 and s2, say, where s1
// and s2 are in the same SNL.  One can then query this structure to find
// out what the situation is for a scalar (transformation illegal,
// scalar expansion req'd at a given level, transformation legal w/o
// scalar expansion) for SNL transformations affecting loops x through s2,
// where s1<=x<=s2.  That is, you ask "if I transform x through to the inner
// of the SNL, is that legal".  The way things are set up, if the answer is
// yes, the scalar is distributable.  Inside the outermost definition of the
// scalar, there must either be no defs of the scalar, or
// all defs and uses must be reductions, else the nest is considered
// untransformable.  TODO OK.  This is conservative.  Thus, we miss
//
//      do i
//        s = 1
//        do j
//          s = -s
//
// which after scalar expansion and distribution, is
//
//      do i
//        s[i] = 1
//      do i
//        do j
//          s[i] = -s[i]
//
// which has the dependence (0,+), which is no problem much of the time.
// So the SX_INFO could be enhanced to handle this simply by
// noting that if it's privatizable at a level and the only other defs and
// uses are one level further in, then expansion is legal (though distribution
// of that inner code, if it's not the inner loop, isn't), etc.
//
//
// SX_PNODE is normally built by Make_Sx_Info(), so
// here I show how the information is used.  The outer possible loop
// being transformed is at depth s1, and the inner at depth s2.
//
//      WN* Wn_Symbol() 
//
//	    A node which belongs to the equivalence class of the nodes
//          being scalar expanded. 
//
//      const SYMBOL& Symbol() const
//
//          The symbol of interest.  For most preciseness, renaming should
//          have been done.  Otherwise, will need to be conservative.
//
//      enum STATUS { ILLEGAL, SE_REQD, SE_NOT_REQD};
//
//      STATUS Transformable(outer, permutation, nloops) const;
//
//          s1 <= outer <= s2.  If we want to transform [outer,s2], but this
//          scalar prevents that, return ILLEGAL.  If this transf doesn't,
//          but scalar expansion is required, return SE_REQD.  In this case
//          there are guaranteed to be no dependences, and distribution of
//          the scalar is legal.  Finally, if the nest is transformable
//          (and optionally distributable) without transformation, return
//          SE_NOT_REQD.
//
//	    If you specify the optional 'permutation' of length 'nloops', 
//          Transformable() will return SE_NOT_REQD in one additional case
//          where without this information it had to return SE_REQD.  This 
//          is the case where the scalar is defined only over the set of 
//          loops whose order does not change in the permutation. 
//
//      INT Expansion_Depth() const
//
//          For when Transformable() returns SE_REQD, this tells you how to
//          expand the scalar.
//          If 0, the outermost def occurs inside the outermost loop.
//          If indeed the loop is scalar expandable, then you must expand
//          this variable s to s[i_{0}, i_{1}, ... i_{Expansion_Depth}].
//          This value is exactly the depth of the "defining definiton".
//          That is, if we have
//                    DO i
//                        x =
//                        DO j
//                            use(x)
//          then the expansion depth is zero.
//
//      BOOL Has_Reductions() const
//
//          True if reduction found.
//
//      WN* Reduction_Carried_By() const
//
//          If Has_Reductions(), the loop_stmt for uses in that reduction
//          E.g.
//              s = 0
//              do i
//                s += ...
//          Reduction_Carried_By() is a pointer to loop i
//
//      void Print(FILE*) const
//
// SX_INFO
//
// Holds all information about scalars.
//
//      SX_PLIST  Plist;
//
//      INT First_Transformable_Depth(const SNL_PLIST_NODE** p =NULL) const
//           If no scalars, returns 0.  Otherwise return largest
//           outer_se_reqd on Plist.  Transformations involving loops
//           with depths smaller than the returned value are illegal.
//           Otherwise, legal.  If p is not NULL, set *p to point to
//           the node that caused the problem.
//
//      SX_PNODE*       Find(const SYMBOL&);
//      const SX_PNODE* Find(const SYMBOL&) const;
//      void            Enter(WN* wn_def, const SYMBOL&, 
//                            WN* reduction_carried_by,
//                            INT se_not_depth, INT se_depth,
//                            INT defining_def_depth);
//      void            Remove(SX_PNODE*);
//            Take this node off the list and free it.
//      void            Print(FILE*) const;
//            Print
//      SX_INFO(MEM_POOL*)
//            Construct one with of these with nothing in it.
//
// TODO: The important thing to fix is homing.  If we have
//     do i
//       t = a(i)
//       do j
//          use(t)
// then t will be expanded to t(i), where it would have been fine to use
// a(i).  Likewise, for
//     do i
//       t = a(i)
//       do j
//          t +=
//       a(i) = t
// one just replaces t with a(i) and removes the non-perfect statements.
// Finally,
//     do i
//       t = 0
//       do j
//          t +=
//       a(i) = t
// one can do the same but replace the upper non-perfect statement with a(i)=0.
//
// TODO OK: If we always scalar expanded, would minvariant always be able
// to remove it?  If so, we could expand all the time.  That would allow us
// to handle the case we talk about missing at the top of these comments.

class SX_PLIST;
class SX_PITER;
class SX_CONST_PITER; 
class SX_PNODE; 
class SX_INFO;

class SX_PNODE : public CHAIN_NODE {
  DECLARE_CHAIN_NODE_CLASS(SX_PNODE);
  friend class SX_PLIST;
  friend class SX_PITER;
  friend class SX_INFO;

  DECL_CXX_ALLOC_AS_FRIEND(SX_PNODE); 

 private:
  WN* _wn_symbol; 
  SYMBOL _symbol;
  BOOL _finalize; 
  BOOL _lcd_depth; 
  WN* _reduction_carried_by;
  mINT8 _outer_se_reqd;
  mINT8 _outer_se_not_reqd;
  mINT8 _defining_def_depth;
  mINT8 _non_red_depth;
  SX_PNODE(WN* wn_sym, const SYMBOL& symbol, WN* reduction_carried_by,
    INT outer_se_reqd, INT outer_se_not_reqd, INT defining_def_depth,
	   INT lcd_depth, BOOL finalize, INT non_red_depth); 
  ~SX_PNODE() {}
 public:
  enum STATUS {ILLEGAL=234, SE_REQD, SE_NOT_REQD};
  WN* Wn_Symbol() const {return _wn_symbol;}
  const SYMBOL& Symbol() const {return _symbol;}
  BOOL Finalize() const {return _finalize;}
  BOOL Lcd_Depth() const {return _lcd_depth;}
  BOOL Has_Reduction() const {return _reduction_carried_by != NULL;}
  WN* Reduction_Carried_By() const {return _reduction_carried_by;}
  void Set_Reduction_Carried_By(WN* wn) {_reduction_carried_by = wn;}
  INT Expansion_Depth() const {return _defining_def_depth;}
  STATUS Transformable(INT depth, INT* permutation = NULL, INT nloops = 0)
    const;
  STATUS Splittable(INT split_depth, INT innermost_depth) const 
    {return innermost_depth < split_depth || _defining_def_depth >= 
       split_depth ? SE_NOT_REQD : SE_REQD; } 
  void Print(FILE* fp) const;
  // normal users don't know what these are
  INT Outer_Se_Reqd() const {return _outer_se_reqd;}
  INT Outer_Se_Not_Reqd() const {return _outer_se_not_reqd;}
};

class SX_PLIST: public CHAIN {
  DECLARE_CHAIN_CLASS(SX_PLIST, SX_PNODE);
 private:
  friend class SX_INFO;
  MEM_POOL* _pool;
 public:
  SX_PLIST(MEM_POOL* pool) : _pool(pool), CHAIN() {}
  void Print(FILE *fp) const;
  ~SX_PLIST();
};

class SX_PITER: public CHAIN_ITER {
  DECLARE_CHAIN_ITER_CLASS(SX_PITER, SX_PNODE, SX_PLIST)
 public:
  ~SX_PITER() {}
};

class SX_CONST_PITER: public CHAIN_ITER {
  DECLARE_CHAIN_CONST_ITER_CLASS(SX_CONST_PITER, SX_PNODE, 
    SX_PLIST)
 public:
  ~SX_CONST_PITER() {}
};

struct SX_INFO {
 public: 
  SX_PLIST Plist;           
  SX_INFO(MEM_POOL* p) : Plist(p) {}
  SX_INFO(const SX_INFO& pinfo, WN* orig, WN* copy, MEM_POOL* pool);
  SX_INFO(const SX_INFO& pinfo, WN* orig, HASH_TABLE<WN*,WN*>* loop_map, 
    MEM_POOL* pool);
  ~SX_INFO() {}
  SX_PNODE* Find(const SYMBOL&);
  const SX_PNODE* Find(const SYMBOL&) const;
  void Enter(WN* wn_def, const SYMBOL&, WN* reduction_carried_by, 
    INT se_not_depth, INT se_depth, INT defining_def_depth, 
	     INT lcd_depth, BOOL finalize, INT non_red_depth=-1);
  void Remove(SX_PNODE*);
  void Print(FILE*) const;
  INT First_Transformable_Depth(const SX_PNODE** = NULL) const;
  INT First_Transformable_Depth_Reduction(const SX_PNODE** = NULL) const;
  void Make_Sx_Info(WN* wn_outer, INT nloops, BOOL ignore_illegal=FALSE);
  void Update_Reduction_Loop_Stmts(WN* wn_inner);
  INT Lcd_Depth();
  BOOL Must_Finalize(); 
 private:
  void Handle_Use(WN* wn_use, INT depth, HASH_TABLE<WN*,BOOL>* loops);
  void Handle_Index_Variable_Def(WN* wn_def, WN* wn_rep_def, 
    INT depth); 
  BOOL Analyze_Reduction(WN* wn_def, INT outer, STACK<WN*>*
    equivalence_class, DOLOOP_STACK* dostack, WN** wn_non_red_def_ptr, 
    INT* non_red_depth_ptr, WN** wn_red_loop_stmt_ptr); 
  void Handle_Other_Def(WN* wn_def, WN* wn_rep_def, INT outer, 
    INT inner, INT depth, DOLOOP_STACK* dostack);
  void Handle_Def(WN* wn_def, WN* wn_rep_def, INT outer, INT inner, 
    INT depth, DOLOOP_STACK* dostack);
  void Walk(WN* wn, INT outer, INT inner, INT depth, 
    HASH_TABLE<WN*,BOOL>* loops, DOLOOP_STACK* dostack);
};

#endif /* sxlist_INCLUDED */
